package utils;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import constants.CommonFolders;
import constants.EStreamAndReader;
import constants.MetadataType;
import model.BackupMetadata;
import model.Metadata;

public final class MetadataUtils {

	public static List<Metadata> findAllFolders(List<Metadata> metadatas) {
		return Lists.newArrayList(Iterables.filter(metadatas,
				new Predicate<Metadata>() {
					@Override
					public boolean apply(Metadata metadata) {
						return metadata.isFolder();
					}
				}));
	}

	public static List<Metadata> findAllFiles(List<Metadata> metadatas) {
		return Lists.newArrayList(Iterables.filter(metadatas,
				new Predicate<Metadata>() {
					@Override
					public boolean apply(Metadata metadata) {
						return !metadata.isFolder()
								&& metadata.getBackupCount() != 0;
					}
				}));
	}

	public static BackupMetadata getRoot(String ownerId) {
		long rootTime = DateUtils.nowUTCTimestamp();
		BackupMetadata root = new BackupMetadata();
		String rootId = StringUtils.getUUID();
		root.setId(rootId);
		root.setParentId("");
		root.setName(ownerId);
		root.setFolder(true);
		root.setSortPriority(0);
		root.setTotalSize(0L);
		root.setOriginName(ownerId);
		root.setModifiedAt(rootTime);
		root.setModifiedBy(ownerId);
		// Root.setBackupCount(1);
		root.setCreatedAt(rootTime);
		root.setCreatedBy(ownerId);
		root.setType("root");
		root.setOwnerId(ownerId);
		return root;
	}

	public static List<Metadata> getInitReset(String rootId, String userId) {
		List<Metadata> lst = new LinkedList<Metadata>();
		List<Metadata> child = new LinkedList<Metadata>();
		List<Metadata> communicationChild = new LinkedList<Metadata>();

		Metadata root = new Metadata();
		Metadata myDocument = new Metadata();
		Metadata myMusic = new Metadata();
		Metadata myPicture = new Metadata();
		Metadata myVideo = new Metadata();
		Metadata myShareFolder = new Metadata();
		Metadata myStorageData = new Metadata();
		Metadata mySyncAppBackupData = new Metadata();
		Metadata myCommunicationData = new Metadata();

		long rootTime = DateUtils.nowUTCTimestamp();
		// String newRootId = StringUtils.getUUID();
		root.setId(rootId);
		root.setParentId("");
		root.setName(userId);
		root.setFolder(true);
		root.setSortPriority(0);
		root.setTotalSize(0L);
		root.setOriginName(userId);
		root.setModifiedAt(rootTime);
		root.setModifiedBy(userId);
		root.setBackupCount(1);
		root.setCreatedAt(rootTime);
		root.setCreatedBy(userId);
		root.setType("root");
		root.setOwnerId(userId);

		myDocument.setType(MetadataType.COMMON.getType());
		myDocument.setSortPriority(1);
		myDocument.setName(CommonFolders.MY_DOCUMENTS.toString());
		myDocument.setOriginName(CommonFolders.MY_DOCUMENTS.getOriginPath());
		
		myMusic.setType(MetadataType.COMMON.getType());
		myMusic.setSortPriority(2);
		myMusic.setName(CommonFolders.MY_MUSIC.toString());
		myMusic.setOriginName(CommonFolders.MY_MUSIC.getOriginPath());

		myPicture.setType(MetadataType.COMMON.getType());
		myPicture.setSortPriority(3);
		myPicture.setName(CommonFolders.MY_PICTURES.toString());
		myPicture.setOriginName(CommonFolders.MY_PICTURES.getOriginPath());

		myVideo.setType(MetadataType.COMMON.getType());
		myVideo.setSortPriority(4);
		myVideo.setName(CommonFolders.MY_VIDEO.toString());
		myVideo.setOriginName(CommonFolders.MY_VIDEO.getOriginPath());

		myShareFolder.setType(MetadataType.SHARECOMMON.getType());
		myShareFolder.setSortPriority(5);
		myShareFolder.setName(CommonFolders.MY_SHARED_FOLDERS.toString());
		myShareFolder.setOriginName(CommonFolders.MY_SHARED_FOLDERS.getOriginPath());

		myStorageData.setType(MetadataType.NORMAL.getType());
		myStorageData.setSortPriority(6);
		myStorageData.setName(CommonFolders.My_STORAGE_DATA.toString());
		myStorageData.setOriginName(CommonFolders.My_STORAGE_DATA.getOriginPath());
		
		mySyncAppBackupData.setType(MetadataType.COMMON.getType());
		mySyncAppBackupData.setSortPriority(7);
		mySyncAppBackupData.setName(CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString());
		mySyncAppBackupData.setOriginName(CommonFolders.MY_SYNC_APP_BACKUP_DATA.getOriginPath());
		mySyncAppBackupData.setVisible(false);
		
		myCommunicationData.setType(MetadataType.COMMUNICATIONCOMMON.getType());
		myCommunicationData.setSortPriority(8);
		myCommunicationData.setName(CommonFolders.MY_COMMUNICATION_DATA.toString());
		myCommunicationData.setOriginName(CommonFolders.MY_COMMUNICATION_DATA.getOriginPath());
		myCommunicationData.setVisible(false);

		lst.add(myDocument);
		lst.add(myMusic);
		lst.add(myPicture);
		lst.add(myVideo);
		lst.add(myShareFolder);
		lst.add(myStorageData);
		lst.add(mySyncAppBackupData);
		lst.add(myCommunicationData);

		for (Metadata item : lst) {
			String uuid = UUID.randomUUID().toString();
			long time = DateUtils.nowUTCTimestamp();
			item.setId(uuid);
			item.setParentId(rootId);
			item.setFolder(true);
			item.setModifiedAt(time);
			item.setModifiedBy(userId);
			item.setBackupCount(-1);
			item.setCreatedAt(time);
			item.setCreatedBy(userId);
			item.setOwnerId(userId);
		}

		Metadata excel = new Metadata();
		Metadata pdf = new Metadata();
		Metadata powerpoint = new Metadata();
		Metadata text = new Metadata();
		Metadata word = new Metadata();
		Metadata zip = new Metadata();
		excel.setName("excel");
		excel.setOriginName("Excel");
		excel.setSortPriority(1);
		pdf.setName("pdf");
		pdf.setOriginName("PDF");
		pdf.setSortPriority(2);
		powerpoint.setName("power point");
		powerpoint.setOriginName("Power Point");
		powerpoint.setSortPriority(3);
		text.setName("text");
		text.setOriginName("Text");
		text.setSortPriority(4);
		word.setName("word");
		word.setOriginName("Word");
		word.setSortPriority(5);
		zip.setOriginName("Zip");
		zip.setName("zip");
		zip.setSortPriority(6);

		child.add(excel);
		child.add(pdf);
		child.add(powerpoint);
		child.add(text);
		child.add(word);
		child.add(zip);

		for (Metadata item : child) {
			String uuid = UUID.randomUUID().toString();
			long time = DateUtils.nowUTCTimestamp();
			item.setId(uuid);
			item.setParentId(myDocument.getId());
			item.setFolder(true);
			item.setTotalSize(0);
			item.setModifiedAt(time);
			item.setModifiedBy(userId);
			item.setBackupCount(-1);
			item.setCreatedAt(time);
			item.setCreatedBy(userId);
			item.setOwnerId(userId);
			item.setType("common");
		}
		
		Metadata chat = new Metadata();
		Metadata contact = new Metadata();
		chat.setName("chat");
		chat.setOriginName("Chat");
		chat.setSortPriority(1);
		contact.setName("contact");
		contact.setOriginName("Contact");
		contact.setSortPriority(2);
		
		communicationChild.add(chat);
		communicationChild.add(contact);
		
		for (Metadata item : communicationChild) {
			String uuid = UUID.randomUUID().toString();
			long time = DateUtils.nowUTCTimestamp();
			item.setId(uuid);
			item.setParentId(myCommunicationData.getId());
			item.setFolder(true);
			item.setTotalSize(0);
			item.setModifiedAt(time);
			item.setModifiedBy(userId);
			item.setBackupCount(-1);
			item.setCreatedAt(time);
			item.setCreatedBy(userId);
			item.setOwnerId(userId);
			item.setType("normal");
		}	
		
		lst.addAll(child);
		lst.addAll(communicationChild);
		lst.add(root);
		return lst;// merge list
	}

	public static List<String> streamSuffix() {
		List<String> list = new ArrayList<String>();
		list.add("3gp");
		list.add("mp4");
		list.add("mpeg");
		list.add("mpg");
		list.add("wmv");
		list.add("asf");
		list.add("avi");
		list.add("flv");
		list.add("mkv");
		list.add("mov");
		list.add("rm");
		list.add("rmvb");
		list.add("mp3");
		list.add("wav");
		list.add("wma");
		list.add("3ga");
		return list;
	}

	public static List<String> readerSuffix() {
		List<String> list = new ArrayList<String>();
		list.add("csv");
		list.add("xls");
		list.add("xlsx");
		list.add("doc");
		list.add("docx");
		list.add("pdf");
		list.add("ppt");
		list.add("pptx");
		list.add("txt");
		list.add("zip");
		list.add("rar");
		list.add("7z");
		list.add("bmp");
		list.add("gif");
		list.add("jpeg");
		list.add("jpg");
		list.add("png");
		return list;
	}

	public static String buildDownLoadURL(String gateway, Metadata metadata) {
		String downLoadUrl = gateway.concat("/file/").concat(
				metadata.getBlockId());
		return downLoadUrl;
	}

	public static String buildSourcePath(Metadata metadata) {
		String srcPath = metadata.getSourcePath();
		String ownerId = metadata.getOwnerId();
		int start = srcPath.indexOf(ownerId) >= 0 ? ownerId.length() + 1 : 0;
		srcPath = srcPath.substring(start, srcPath.length());
		return srcPath;
	}

	public static String getAuthorization(String userId, String token) {
		return StringUtils.encodeBase64(userId.concat(":").concat(token));
	}

	public static String getConvertedExtention(Metadata metadata, EStreamAndReader type) {
		String suffix = metadata
				.getName()
				.substring(metadata.getName().lastIndexOf(".") + 1,
						metadata.getName().length()).toLowerCase();
		String result = "";
		if (type.equals(EStreamAndReader.STREAMING)) {
			if (StringUtils.isNullOrEmpty(suffix)) {
				result = ".mp4";
			} else {
				if (streamSuffix().contains(suffix)) {
					result = ".".concat(suffix);
				} else {
					result = ".mp4";
				}
			}
		} else if (type.equals(EStreamAndReader.READER)) {
			if (readerSuffix().contains(suffix)) {
				result = ".".concat(suffix);
			}
		}
		return result;
	}

	// userId/my backup data/xxx to userId/xxx
	public static String fullBackupPathToNormalPath(String fullBackupPath) {
		return fullBackupPath.replaceFirst("([^/]+)/my backup data(/|$)", "$1$2");
	}

}