package service.impl;

import java.util.List;

import javax.annotation.PostConstruct;

import model.SQSDeviceUnregisterMessageBody;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import service.DeviceService;
import utils.StringUtils;

import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;

import constants.HttpStatus;
import exception.MetadataException;

@Service
public class DeviceUnregisterServiceImpl implements Runnable {
	private static final Logger logger = LogManager.getLogger(DeviceUnregisterServiceImpl.class);

	@Value("${sqs.sso.queueName}")
	private String queueName;
	@Value("${sqs.deviceUnregister.sleepInMilliseconds}")
	private long sleepInMilliseconds;
	@Value("${sqs.deviceUnregister.action}")
	private String action;

	@Autowired
	private AmazonSQSClient sqsClient;
	@Autowired
	private DeviceService deviceService;

	@PostConstruct
	private void init() {
		Thread thread = new Thread(this);
		thread.setDaemon(true);
		thread.start();
	}

	@Override
	public void run() {
		String queueUrl = sqsClient.getQueueUrl(queueName).getQueueUrl();

		while (true) {
			try {
				handle(queueUrl);
				Thread.sleep(sleepInMilliseconds);
			} catch (Throwable t) {
				logger.warn("handle error", t);
			}
		}
	}

	private void handle(String queueUrl) {
		List<Message> messages = sqsClient.receiveMessage(new ReceiveMessageRequest(queueUrl).withMaxNumberOfMessages(1)).getMessages();
		if (messages.isEmpty()) {
			return;
		}
		Message message = messages.get(0);
		String receiptHandle = message.getReceiptHandle();
		SQSDeviceUnregisterMessageBody body = StringUtils.readJSON(message.getBody(), SQSDeviceUnregisterMessageBody.class);
		if (body != null && body.getAction().equals(action)) {
			try {
				deviceService.unregister(body.getUserId(), body.getDeviceId());
			} catch (MetadataException me) {
				if (me.getHttpStatus() == HttpStatus.DEVICE_NOT_EXIST) {
					logger.warn("unregister error", me);
				} else {
					throw me;
				}
			}
		}
		sqsClient.deleteMessage(queueUrl, receiptHandle);
	}
}
