package service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;

import model.BaseMetadata;
import model.MQtask;
import model.MessageForDecrypt;
import model.MessageForEncrypt;
import model.MessageForFullTextSearch;
import model.MessageForReader;
import model.MessageForStreaming;
import model.MessageForUnzip;
import model.MessageForZip;
import model.Metadata;
import model.ZipSourcePure;
import model.request.EncryptRequest;
import model.request.SendAllMetadataToMQRequest;
import model.request.UnzipRequest;
import model.request.ZipRequest;
import model.request.ZipSource;

import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import service.MQService;
import utils.MetadataUtils;
import utils.StringUtils;

import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.SendMessageBatchRequest;
import com.amazonaws.services.sqs.model.SendMessageBatchRequestEntry;

import constants.EStreamAndReader;
import dao.MetadataDao;
import factory.MetadataFactory;
import factory.PathFactory;
import factory.impl.MQMuitiThreadMessageSender;
import factory.impl.ThreadPool;

@Service
public class MQServiceImpl implements MQService {

	private static final Logger logger = LogManager
			.getLogger(EmailServiceImpl.class);

	@Value("${gateway.url}")
	private String gatewayUrl;
	@Value("${adminToken}")
	private String adminToken;
	@Value("${mq.streamingQueueName}")
	private String streamingQueueName;
	@Value("${mq.readerQueueName}")
	private String readerQueueName;
	@Value("${mq.fullTextSearchQueueName}")
	private String fullTextSearchQueueName;
	@Value("${mq.jobQueueName}")
	private String jobQueueName;
	@Value("${mq.thumbnailQueueName}")
	private String thumbnailQueueName;
	@Value("${mq.once_send_count}")
	private int size;

	@Autowired
	private PathFactory pathFactory;
	@Autowired
	private MetadataFactory metadataFactory;
	@Autowired
	private AmazonSQSClient client;
	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private ThreadPool threadPool;

	@Override
	public void sendForStreaming(final Metadata metadata, final String token) {
		if (metadata.isStreamingFile(pathFactory) && !metadata.isEncrypted()) {
			Thread thread = new Thread() {
				@Override
				public void run() {
					String authorization = generateAuthorization(metadata.getOwnerId(),
							adminToken);
					String path = metadataFactory.getMetadataOriginalPath(metadata);
					String extentsion = FilenameUtils.getExtension(metadata.getName());
					sendForStreaming(metadata.generateDownloadUrl(gatewayUrl),
							metadata.generateStreamingBaseUrl(gatewayUrl), extentsion,
							path, metadata.getSourcePath(), metadata.getOwnerId(),
							token, authorization);
				}
			};
			threadPool.execute(thread);
		}
	}
	
	private void sendForStreaming(List<Metadata> metadataList, String token) {
		for (Metadata metadata : metadataList) {
			sendForStreaming(metadata, token);
		}
	}
	
	@Override
	public void sendForReader(final Metadata metadata, final String token) {
		if (metadata.isReaderFile(pathFactory) && !metadata.isEncrypted()) {
			Thread thread = new Thread() {
				@Override
				public void run() {
					String authorization = generateAuthorization(metadata.getOwnerId(),
							adminToken);
					String path = metadataFactory.getMetadataOriginalPath(metadata);
					String extentsion = FilenameUtils.getExtension(metadata.getName());
					sendForReader(metadata.generateDownloadUrl(gatewayUrl),
							metadata.generateReaderBaseUrl(gatewayUrl), extentsion,
							path, metadata.getSourcePath(), metadata.getOwnerId(),
							token, authorization);
				} 
			};
			threadPool.execute(thread);
		}
	}
	
	private void sendForReader(List<Metadata> metadataList, String token) {
		for (Metadata metadata : metadataList) {
			sendForReader(metadata, token);
		}
	}

	@Override
	public void sendAddFullTextSearch(final BaseMetadata metadata) {
		if (metadata.isReaderFile(pathFactory) && !metadata.isEncrypted()) {
			Thread thread = new Thread() {
				@Override
				public void run() {
					sendForFullTextSearch("add", metadata.getOwnerId(),
							metadata.getBlockId(),
							metadata.generateDownloadUrl(gatewayUrl));
				}
			};
			threadPool.execute(thread);
		}
	}

	private void sendAddFullTextSearch(List<? extends BaseMetadata> metadataList) {
		for (BaseMetadata metadata : metadataList) {
			sendAddFullTextSearch(metadata);
		}
	}

	private String generateAuthorization(String userId, String token) {
		return StringUtils.encodeBase64(String.format("%s:%s", userId, token));
	}

	private void sendForStreaming(String downloadUrl, String baseUrl,
			String extension, String path, String sourcePath, String ownerId,
			String neweggboxSsoToken, String authorization) {

		MessageForStreaming message = new MessageForStreaming(downloadUrl,
				baseUrl, extension, path, sourcePath, ownerId,
				neweggboxSsoToken, authorization);
		sendMessage(streamingQueueName, StringUtils.writeJSON(message));
	}

	private void sendForReader(String downloadUrl, String baseUrl,
			String extension, String path, String sourcePath, String ownerId,
			String neweggboxSsoToken, String authorization) {

		MessageForReader message = new MessageForReader(downloadUrl, baseUrl,
				extension, path, sourcePath, ownerId, neweggboxSsoToken,
				authorization);
		sendMessage(readerQueueName, StringUtils.writeJSON(message));
	}

	private void sendForFullTextSearch(String type, String ownerId,
			String blockId, String fileUrl) {
		
		MessageForFullTextSearch message = new MessageForFullTextSearch(type,
				ownerId, blockId, fileUrl);
		sendMessage(fullTextSearchQueueName, StringUtils.writeJSON(message));
	}

	@Override
	public void sendForThumbnail(final BaseMetadata metadata) {
		if (metadata.isPicture(pathFactory) && !metadata.isEncrypted()) {
			Thread thread = new Thread() {
				@Override
				public void run() {
					Map<String, String> message = new HashMap<String, String>();
					message.put("block_id", metadata.getBlockId());
					sendMessage(thumbnailQueueName, StringUtils.writeJSON(message));
				}
			};
			threadPool.execute(thread);
		}
	}

	@Override
	public void sendForZip(ZipRequest zipRequest) {
		List<ZipSourcePure> zipSourceList = new ArrayList<ZipSourcePure>();
		for (ZipSource zipSource : zipRequest.getZipSource()) {
			zipSourceList.add(zipSource.toZipSourcePure());
		}
		String authorization = generateAuthorization(zipRequest.getUserId(),
				adminToken);

		MessageForZip message = new MessageForZip("pack",
				zipRequest.getTargetPath(), "", zipRequest.getTargetOwnerId(),
				zipRequest.getTargetSharedRootId(), zipSourceList,
				zipRequest.getUserId(), zipRequest.getPassword(),
				zipRequest.getToken(), authorization);
		sendMessage(jobQueueName, StringUtils.writeJSON(message));
	}

	@Override
	public void sendForUnzip(UnzipRequest unzipRequest) {
		String authorization = generateAuthorization(unzipRequest.getUserId(),
				adminToken);

		MessageForUnzip message = new MessageForUnzip("unpack",
				unzipRequest.getPath(), unzipRequest.getSourcePath(),
				unzipRequest.getOwnerId(), unzipRequest.getSharedRootId(),
				unzipRequest.getTargetPath(), "",
				unzipRequest.getTargetOwnerId(),
				unzipRequest.getTargetSharedRootId(), unzipRequest.getUserId(),
				unzipRequest.getPassword(), unzipRequest.getToken(),
				authorization);
		sendMessage(jobQueueName, StringUtils.writeJSON(message));
	}

	public void sendForEncrypt(boolean isFolder, EncryptRequest encryptRequest) {
		MessageForEncrypt message = new MessageForEncrypt(isFolder,
				encryptRequest.getPath(), encryptRequest.getSourcePath(),
				encryptRequest.getOwnerId(), encryptRequest.getSharedRootId(),
				encryptRequest.getPassword(), encryptRequest.getUserId(),
				encryptRequest.getToken(), generateAuthorization(
						encryptRequest.getUserId(), adminToken));
		sendMessage(jobQueueName, StringUtils.writeJSON(message));
	}

	public void sendForDecrypt(EncryptRequest decryptRequest) {
		MessageForDecrypt message = new MessageForDecrypt(
				decryptRequest.getPath(), decryptRequest.getSourcePath(),
				decryptRequest.getOwnerId(), decryptRequest.getSharedRootId(),
				decryptRequest.getPassword(), decryptRequest.getUserId(),
				decryptRequest.getToken(), generateAuthorization(
						decryptRequest.getUserId(), adminToken));
		sendMessage(jobQueueName, StringUtils.writeJSON(message));
	}

	private void sendMessage(String queueName, String message) {
		if (message == null) {
			return;
		}
		try {
			client.sendMessage(queueName, message);
		} catch (Exception e) {
			logger.error(String.format("queueName : %s, message : %s",
					queueName, message), e);
		}
	}

	private boolean batchSendMessage(List<Metadata> list,
			ExecutorService executor, String queueName, EStreamAndReader type,
			SendAllMetadataToMQRequest request) {
		boolean result = true;
		try {
			List<SendMessageBatchRequest> requests = prepareBatchRequest(list,
					queueName, type, request);
			CountDownLatch threadSignal = new CountDownLatch(requests.size());
			for (SendMessageBatchRequest task : requests) {
				MQMuitiThreadMessageSender t = new MQMuitiThreadMessageSender(
						task, client, threadSignal);
				 executor.execute(t);
			}
			threadSignal.await();// wait all thread finished
		} catch (Exception e) {
			result = false;
			logger.error(String.format("batch send metadata Exception:%s", e));
		}
		return result;
	}

	private String buildStreamMsg(Metadata metadata,
			SendAllMetadataToMQRequest request) {
		String downLoadUrl = MetadataUtils.buildDownLoadURL(gatewayUrl,
				metadata);
		String baseUrl = metadata.generateStreamingBaseUrl(gatewayUrl);
		String extention = MetadataUtils.getConvertedExtention(metadata,
				EStreamAndReader.STREAMING);
		String filePath = metadata.getOriginName().substring(
				metadata.getOriginName().indexOf("/") + 1,
				metadata.getOriginName().length());
		String path = filePath.toLowerCase();
		String sourcePath = MetadataUtils.buildSourcePath(metadata);
		String ownerId = metadata.getOwnerId();
		String authorization = MetadataUtils.getAuthorization(
				request.getUserId(), request.getToken());// any one
		String token = request.getToken();

		String message = String
				.format("{\"download_url\":\"%s\",\"base_url\":\"%s\",\"extension\":\"%s\","
						+ "\"path\":\"%s\",\"source_path\":\"%s\",\"owner_id\":\"%s\","
						+ "\"authorization\":\"%s\",\"neweggbox_sso_token\":\"%s\"}",
						downLoadUrl, baseUrl, extention, path, sourcePath,
						ownerId, authorization, token);
		return message;
	}

	private String buildReaderMsg(Metadata metadata,
			SendAllMetadataToMQRequest request) {
		String downLoadUrl = MetadataUtils.buildDownLoadURL(gatewayUrl,
				metadata);
		String baseUrl = metadata.generateReaderBaseUrl(gatewayUrl);
		String extention = MetadataUtils.getConvertedExtention(metadata,
				EStreamAndReader.READER);

		String path = metadata
				.getOriginName()
				.substring(metadata.getOriginName().indexOf("/") + 1,
						metadata.getOriginName().length()).toLowerCase();
		String sourcePath = MetadataUtils.buildSourcePath(metadata);
		String ownerId = metadata.getOwnerId();
		String authorization = MetadataUtils.getAuthorization(
				request.getUserId(), request.getToken());// any one
		String token = request.getToken();

		String message = String
				.format("{\"download_url\":\"%s\",\"base_url\":\"%s\",\"extension\":\"%s\","
						+ "\"path\":\"%s\",\"source_path\":\"%s\",\"owner_id\":\"%s\","
						+ "\"authorization\":\"%s\",\"neweggbox_sso_token\":\"%s\"}",
						downLoadUrl, baseUrl, extention, path, sourcePath,
						ownerId, authorization, token);
		return message;
	}

	private List<SendMessageBatchRequest> prepareBatchRequest(
			List<Metadata> list, String queueName, EStreamAndReader type,
			SendAllMetadataToMQRequest req) {
		List<SendMessageBatchRequest> list_request = new ArrayList<SendMessageBatchRequest>();
		int Index = list.size() + (10 - list.size() % 10);
		for (int i = 0; i <= Index; i += 10) {
			List<Metadata> tempList = new ArrayList<Metadata>();
			if (i > 0) {
				for (int j = i; j > 0 && j > i - 10; j--) {
					if (j >= list.size()) {
						continue;
					}
					tempList.add(list.get(j));
				}
			} else {
				tempList.add(list.get(0));
			}
			List<SendMessageBatchRequestEntry> entries = new ArrayList<SendMessageBatchRequestEntry>();
			SendMessageBatchRequest request = new SendMessageBatchRequest();
			for (Metadata metadata : tempList) {
				SendMessageBatchRequestEntry entry = new SendMessageBatchRequestEntry();
				String message = "";
				if (EStreamAndReader.STREAMING.equals(type)) {
					message = buildStreamMsg(metadata, req);
				} else if (EStreamAndReader.READER.equals(type)) {
					message = buildReaderMsg(metadata, req);
				}
				entry.setMessageBody(message);
				entry.setId(metadata.getId());
				entries.add(entry);
			}
			if (entries.size() > 0) {
				request.setQueueUrl(queueName);
				request.setEntries(entries);
				list_request.add(request);
			}
		}
		return list_request;
	}

	// 300 items per batch
	private List<List<Metadata>> prepareTask(List<Metadata> list) {
		if (list == null || list.size() == 0) {
			return null;
		}
		List<List<Metadata>> taskList = new ArrayList<List<Metadata>>();
		int Index = list.size() + (300 - list.size() % 300);
		for (int i = 0; i <= Index; i += 300) {
			List<Metadata> tempList = new ArrayList<Metadata>();
			if (i > 0) {
				for (int j = i; j > 0 && j > i - 300; j--) {
					if (j >= list.size()) {
						continue;
					}
					tempList.add(list.get(j));
				}
			} else {
				tempList.add(list.get(0));
			}
			taskList.add(tempList);
		}
		if (taskList.size() > 1) {
			List<Metadata> first = taskList.get(0);// to merge head and tail
			List<Metadata> last = taskList.get(taskList.size() - 1);
			taskList.remove(first);
			taskList.remove(last);
			first.addAll(last);
			taskList.add(first);// merge the head and tail
		}
		return taskList;
	}

	private MQtask filtMetadata(List<Metadata> list, String[] extensions) {
		MQtask mqtask = new MQtask();
		List<Metadata> readerTask = new ArrayList<Metadata>();
		List<String> readerSuffix = MetadataUtils.readerSuffix();
		List<Metadata> streamTask = new ArrayList<Metadata>();
		List<String> streamSuffix = MetadataUtils.streamSuffix();
		for (Metadata item : list) {
			String suffix = item
					.getName()
					.substring(item.getName().lastIndexOf(".") + 1,
							item.getName().length()).toLowerCase();
			if (readerSuffix.contains(suffix)) {
				if (extensions == null || extensions.length == 0) {
					readerTask.add(item);
				} else {
					for (String extension : extensions) {
						if (extension.trim().equals(suffix)) {
							readerTask.add(item);
						}
					}
				}
			} else if (streamSuffix.contains(suffix)) {
				if (extensions == null
						|| (extensions.length == 1 && "".equals(extensions[0]))) {
					streamTask.add(item);
				} else {
					for (String extension : extensions) {
						if (extension.equals(suffix)) {
							streamTask.add(item);
						}
					}
				}
			}
		}
		mqtask.setReaderTask(readerTask);
		mqtask.setStreamTask(streamTask);
		return mqtask;
	}

	@Override
	public boolean sendAllMetadata(SendAllMetadataToMQRequest request) {
		boolean result = false;
		int count = metadataDao.getMetadataCount();
		int pageCount = count % size > 0 ? count / size + 1 : count / size;
		// query data
		for (int pageIndex = 1; pageIndex <= pageCount; pageIndex++) {
			List<Metadata> list = metadataDao.batchGetMetadata((pageIndex - 1)
					* size, pageIndex * size);
			// filter data
			MQtask mqTask = filtMetadata(list, request.getExtensions());
			if (mqTask.getReaderTask() == null
					&& mqTask.getStreamTask() == null) {
				return false;
			}
			if (StringUtils.isNullOrEmpty(request.getFilter())) {
				sendForReader(mqTask.getReaderTask(), request.getToken());
				sendForStreaming(mqTask.getStreamTask(), request.getToken());
			} else {
				if (request.getFilter().equals("reader")) {
					sendForReader(mqTask.getReaderTask(), request.getToken());
				} else if (request.getFilter().equals("streaming")) {
					sendForStreaming(mqTask.getStreamTask(), request.getToken());
				}
			}		

			// send Full Text Search MQ
			sendAddFullTextSearch(list);
		}
		return result;
	}
}
