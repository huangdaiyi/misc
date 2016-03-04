package service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;

import model.ActiveHistory;
import model.BackupMetadata;
import model.BaseMetadata;
import model.CountManageModel;
import model.Device;
import model.Metadata;
import model.MetadataAttr;
import model.PathInfo;
import model.UserSetting;
import model.request.GetDeviceRequest;
import model.request.GetUserRequest;
import model.request.RestCommonRequest;
import model.request.RestUserRequest;
import model.request.UpdateSettingsRequest;
import model.request.UpdateUserDiskSpaceRequest;
import model.response.GetDeviceResponse;
import model.response.GetUserResponse;
import model.response.SSOUserInfoResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import service.BackupMetadataService;
import service.CountService;
import service.DeviceService;
import service.DynamoDBService;
import service.FolderService;
import service.MetadataAttrService;
import service.SSOService;
import service.UserService;
import service.XmppService;
import utils.DateUtils;
import utils.MathUtils;
import utils.MetadataUtils;
import utils.StringUtils;
import utils.TypeConvert;
import constants.ActivityHistoryLevel;
import constants.ChangeReason;
import constants.CommonFolders;
import constants.EactivityHistoryType;
import constants.HttpStatus;
import constants.MetadataType;
import constants.SystemEvent;
import dao.CountDao;
import dao.DeviceDao;
import dao.MetadataDao;
import dao.SyncRelationDao;
import dao.UserDao;
import exception.MetadataException;
import factory.MetadataFactory;

@Service
public class UserServiceImpl implements UserService {

	private static final Logger logger = LogManager.getLogger(UserServiceImpl.class);

	@Autowired
	private SSOService ssoService;
	@Autowired
	private DeviceService deviceService;
	@Autowired
	private BackupMetadataService backupMetadataService;
	@Autowired
	private MetadataAttrService metadataAttrService;
	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private UserDao userDao;
	@Autowired
	private CountDao countDao;
	@Autowired
	private DeviceDao deviceDao;
	@Autowired
	private DynamoDBService dynamoDBService;
	@Autowired
	private CountService countService;
	@Autowired
	private XmppService xmppService;
	@Autowired
	private FolderService folderService;
	@Autowired
	private MetadataFactory metadataFactory;
	@Autowired
	private SyncRelationDao syncRelationDao;
	
	@Value("${default.user.total}")
	private Long userTotal;
	@Value("${aes.payment.secretkey}")
	private String aesPaymentSecretKey;
	@Value("${aes.payment.ivparam}")
	private String aesPaymentIvParam;

	@Override
	@Transactional
	public GetUserResponse getUser(GetUserRequest getUserRequest)
			throws MetadataException {
		SSOUserInfoResponse ssoUserResponse = ssoService
				.getUserInfoByToken(getUserRequest);
		GetUserResponse user = ssoUserResponse.toUser();
		getUserRequest.setDeviceId(ssoUserResponse.getDeviceId());
		GetUserResponse userInfo = userDao.getUser(user.getId());
		if (userInfo == null) {
			// initialize user,if user not exist
			userInfo = createUser(user);
		}
		MetadataAttr root = countDao.queryCount(metadataDao
				.getRootMetadataId(userInfo.getId()));
		MetadataAttr backupRoot = countDao.queryCount(backupMetadataService
				.getBackupIndexByPath("", user.getId()));
		getUserRequest.setDeviceId(getUserRequest.getDeviceId());
		String deviceUid = getDeviceUID(getUserRequest);
		user.setSettings(userInfo.getSettings() != null ? userInfo.getSettings() : "");
		user.setBackupSize(backupRoot.getTotalSize());
		user.setTotal(userInfo.getTotal());
		user.setUsage(root.getTotalSize() + backupRoot.getTotalSize());
		user.setAvailable(MathUtils.toPositive(user.getTotal() - user.getUsage()));
		user.setSettings(userInfo.getSettings());
		user.setDeviceUid(deviceUid);
		user.setLastEditTime(userInfo.getLastEditTime());
		
		try {
			Metadata chatFolder = metadataDao.getMetadataByPath(CommonFolders.MY_COMMUNICATION_DATA_CHAT.toString().toLowerCase(), "", "");
			user.setChatUsage(chatFolder == null ? 0 : chatFolder.getTotalSize());
		} catch(Exception ex) {
			user.setChatUsage(0);
		}
		
		try {
			Metadata contactFolder = metadataDao.getMetadataByPath(CommonFolders.MY_COMMUNICATION_DATA_CONTACT.toString().toLowerCase(), "", "");
			user.setContactUsage(contactFolder == null ? 0 : contactFolder.getTotalSize());
		} catch(Exception ex) {
			user.setContactUsage(0);
		}
		
		return user;
	}

	@Override
	@Transactional
	public boolean reset(RestUserRequest restUserRequest)
			throws MetadataException {
		List<BaseMetadata> allNode = metadataDao
				.getUserAllNodeByOwnerId(restUserRequest.getUserId());// query
		BackupMetadata root = MetadataUtils
				.getRoot(restUserRequest.getUserId());
		String rootId = StringUtils.getUUID();
		root.setOriginalIndexId(rootId);
		List<Metadata> metaList = MetadataUtils.getInitReset(rootId,
				restUserRequest.getUserId());
		
		List<String > sharedIds = new ArrayList<String>();
		for (BaseMetadata baseMetadata : allNode) {
			if (baseMetadata.getType().equals(MetadataType.SHARE.toString())) {
				sharedIds.add(baseMetadata.getId());
			}
		}

		List<String> idList = getIdList(allNode);
		if (allNode.size() > 0) {
			userDao.updateCollaborate(sharedIds);// update
			metadataDao.batchDeleteMetadata(idList);// delete
			countDao.delete(idList);// delete
			userDao.deleteAllMobileFolders(restUserRequest.getUserId());// delete
			userDao.deleteBackupInAttr(restUserRequest.getUserId());
			userDao.deleteAllBackupData(restUserRequest.getUserId());// delete
		}
		backupMetadataService.registerRootBackup(root);
		metadataDao.batchCreateMetadata(metaList);// create
		
		// clean settings of user info
		UserSetting userSetting = new UserSetting();
		userSetting.setSettings(null);
		userSetting.setUser(restUserRequest.getUserId());
		userDao.updateSetting(userSetting);
		
		// clean settings of device info
		for(Device device : deviceDao.getDevices(restUserRequest.getUserId())) {
			deviceDao.update(device.getUserId(), device.getDeviceUid(), null);
		}
		
		// create attr & update parents' size/count
		List<MetadataAttr> attrList = new ArrayList<MetadataAttr>();
		CountManageModel countManageModel = new CountManageModel();
		for (Metadata folder : metaList) {
			attrList.add(new MetadataAttr(folder.getId(), false));
			countManageModel.addEmptyFolder(folder.getParentId());
		}
		metadataAttrService.batchCreateMetadataAttr(attrList);
		countService.updateAllParentCount(countManageModel);
		
		//delete ref link & update parents' count
		if (!sharedIds.isEmpty()) {
			List<Metadata> refMetadatas = metadataDao.getRefMetadataBySharedId(sharedIds);
			for (Metadata metadata : refMetadatas) {
				metadataDao.deleteMetadata(metadata.getId());
				CountManageModel countModel = new CountManageModel();
				countModel.deleteFolder(metadata.getParentId(), 0, 0, 1, 0, 0, 1);
				countService.updateAllParentCount(countModel);
			}
		}
		
				
		folderService.resetGlobalIconByUser(restUserRequest.getUserId());
		List<ActiveHistory> history = convertActiveHistory(metaList,
				restUserRequest.getToken(), EactivityHistoryType.RESET_USER,
				null);
		backupMetadataService.backupMetadata(metaList,
				restUserRequest.getUserId());// backup
		syncRelationDao.deleteSyncRelation(syncRelationDao.
				getSyncRelationsByUserId(restUserRequest.getUserId())); // SyncRelations
		dynamoDBService.createActivityHistory(history);// activityHistory
		xmppService.sendToAllDevices(SystemEvent.RESET_USER, restUserRequest.getUserId());
		return true;
	}

	@Override
	@Transactional
	public boolean resetCommon(RestCommonRequest restCommonRequest)
			throws MetadataException {
		boolean result = false;
		if (StringUtils.isNullOrEmpty(restCommonRequest.getPath())) {
			List<Metadata> metaList = userDao
					.getAllCommonFiles(restCommonRequest);// query
			result = resetCommonMethod(metaList, restCommonRequest,
					EactivityHistoryType.RESET_COMMON_ALL);
		} else {
			String path = getPathName(restCommonRequest.getPath());
			//restCommonRequest.setPath(path);
			if (path.equalsIgnoreCase((CommonFolders.MY_DOCUMENTS.toString()))) {
				List<Metadata> metaList = userDao
						.getMyDocumentFileChild(restCommonRequest);// query
				result = resetCommonMethod(metaList, restCommonRequest,
						EactivityHistoryType.RESET_COMMON);
			} else {
				List<Metadata> metaList = userDao
						.getOridinaryCommonFolderChild(restCommonRequest);
				result = resetCommonMethod(metaList, restCommonRequest,
						EactivityHistoryType.RESET_COMMON);
			}
		}

		if (result) {
			PathInfo pathInfo = new PathInfo(restCommonRequest.getUserId(), StringUtils.concatFilePath(restCommonRequest.getUserId(), restCommonRequest.getPath()), true);
			xmppService.sendToAllDevices(SystemEvent.RESET_COMMON, pathInfo);
			// send dynamoDB history
			BaseMetadata metadata = metadataFactory.getBaseMetadataByPath(pathInfo);
			dynamoDBService.createActivityHistory(restCommonRequest.getPath(), "",
					"", "", metadata.getId(), pathInfo.getUserId(),
					DateUtils.nowUTCDateTime(),
					ChangeReason.RESET_COMMON_FOLDER.toString(),
					metadata.getSharedRootId(),
					ActivityHistoryLevel.NORMAL.toString());
		}

		return result;
	}

	@Transactional
	private boolean resetCommonMethod(List<Metadata> metaList,
			RestCommonRequest restCommonRequest, EactivityHistoryType type) {
		metaList = filterRestTarget(metaList, restCommonRequest.getDeviceUid());
		List<String> idList = getIdList(metaList);
		if (idList.size() == 0) {
			return false;
		}
		userDao.updateCollaborate(idList);// update
		metadataDao.batchDeleteMetadata(idList);// delete
		countDao.delete(idList);// delete
		
		//countService.recalculateForDeletingFiles(metaList);
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		for (Metadata metadata : metaList) {
			countManageModel.deleteFile(metadata.getParentId(), metadata.getSize());
		}
		countService.updateAllParentCount(countManageModel); 
		// unbackup file
		backupMetadataService.unbackupMetadatas(idList);
		
		deleteMobileFolder(restCommonRequest.getUserId(),
				restCommonRequest.getDeviceUid());// delete
		
		
		
		return true;
	}

	@Override
	public boolean updateSettings(UpdateSettingsRequest updateSettingsRequest)
			throws MetadataException {
		UserSetting setting = new UserSetting();
		setting.setSettings(updateSettingsRequest.getSettings());
		setting.setUser(updateSettingsRequest.getUserId());
		if (StringUtils.isNullOrEmpty(setting.getSettings())
				|| userDao.updateSetting(setting) <= 0) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return true;
	}

	@Override
	public void updateUserDiskSpace(UpdateUserDiskSpaceRequest updateUserDiskSpaceRequest) throws MetadataException {
		String decryptedAccessKey = StringUtils.decryptAES128(aesPaymentSecretKey, aesPaymentIvParam, updateUserDiskSpaceRequest.getAccessKey());
		if (decryptedAccessKey == null || decryptedAccessKey.isEmpty() || decryptedAccessKey.equals(updateUserDiskSpaceRequest.getUserId()) == false) {
			throw new MetadataException(HttpStatus.CANT_ACCESS_API);
		}
		String userId = updateUserDiskSpaceRequest.getUserId();
		Long total = updateUserDiskSpaceRequest.getTotal();
		logger.info("updateUserDiskSpace {}: {}", userId, total);
		userDao.updateUserDiskSpace(userId, total);
		xmppService.sendToAllDevices(SystemEvent.UPDATE_USER_DISK_SPACE, userId);
		dynamoDBService.createActivityHistory(userId.concat("/"), "", "", "", "", userId, DateUtils.nowUTCDateTime(), SystemEvent.UPDATE_USER_DISK_SPACE.toString(), "", ActivityHistoryLevel.SYSTEM.toString());
	}

	private GetUserResponse createUser(GetUserResponse user) {
		user.setTotal(this.userTotal);
		userDao.createUser(user);

		List<Metadata> folders = new LinkedList<Metadata>();
		String rootUUID = StringUtils.getUUID();
		// create common folders
		int sortPriority = 0;
		Metadata rootSetting = buildUserMetadata(rootUUID, "",
				MetadataType.COMMON, user.getId(), user.getId(), 0L,
				user.getId(), sortPriority, true);
		folders.add(rootSetting);
		BackupMetadata backupRoot = TypeConvert.convert(rootSetting, "", "");

		String documentId = null;
		String communicationId = null;
		MetadataType folderType = MetadataType.COMMON;
		sortPriority = 1;
		for (CommonFolders common : CommonFolders.values()) {
			if (common.toString().startsWith(CommonFolders.MY_DOCUMENTS.toString().concat("/"))) {
				continue;
			} else if (common.toString().startsWith(CommonFolders.MY_COMMUNICATION_DATA.toString().concat("/"))) {
				continue;
			} else if (common == CommonFolders.MY_DEVICE_SYNC_FOLDERS) {
				continue;
			} else if (common == CommonFolders.MY_BACKUP_DATA) {
				continue;
			}
			
			String metadataId = StringUtils.getUUID();
			boolean isVisible = true;
			folderType = MetadataType.COMMON;
			
			switch (common) {
				case MY_DOCUMENTS:
					documentId = metadataId;
					folderType = MetadataType.COMMON;
					break;
				case MY_SHARED_FOLDERS:
					folderType = MetadataType.SHARECOMMON;
					break;
				case MY_SYNC_APP_BACKUP_DATA:
					isVisible = false;
					break;
				case My_STORAGE_DATA:
					folderType = MetadataType.NORMAL;
					break;
				case MY_COMMUNICATION_DATA:
					communicationId = metadataId;
					folderType = MetadataType.COMMUNICATIONCOMMON;
					isVisible = false;
					break;
				default:
					break;
			}
			
			Metadata commonFoldersSetting = buildUserMetadata(metadataId,
					rootUUID, folderType, common.toString(),
					common.getOriginPath(), 0L, user.getId(), sortPriority, isVisible);
			folders.add(commonFoldersSetting);
			sortPriority += 1;
		}

		// under CommonFolders.MY_DOCUMENTS
		folderType = MetadataType.COMMON;
		sortPriority = 1;
		for (CommonFolders.DocumentFolders document : CommonFolders.DocumentFolders.values()) {
			Metadata documentFoldersSetting = buildUserMetadata(
					StringUtils.getUUID(), documentId, folderType,
					document.toString(), document.getOriginName(), 0L,
					user.getId(), sortPriority, true);
			folders.add(documentFoldersSetting);
			sortPriority += 1;
		}
		
		// under CommonFolders.MY_COMMUNICATION_DATA
		folderType = MetadataType.NORMAL;
		sortPriority = 1;
		for (CommonFolders common : CommonFolders.values()) {
			if (common.toString().startsWith(CommonFolders.MY_COMMUNICATION_DATA.toString().concat("/"))) {
				String folderName = common.getOriginPath().replace(CommonFolders.MY_COMMUNICATION_DATA.getOriginPath().concat("/"), "");
				Metadata communicationFoldersSetting = buildUserMetadata(
						StringUtils.getUUID(), communicationId, folderType,
						folderName.toLowerCase(), folderName, 0L,
						user.getId(), sortPriority, true);
				folders.add(communicationFoldersSetting);
				sortPriority += 1;
			}
		}
				
		// create all commons
		metadataDao.batchCreateMetadata(folders);
		
		// create attr & update parents' size/count
		List<MetadataAttr> attrList = new ArrayList<MetadataAttr>();
		CountManageModel countManageModel = new CountManageModel();
		for (Metadata folder : folders) {
			attrList.add(new MetadataAttr(folder.getId(), false));
			countManageModel.addEmptyFolder(folder.getParentId());
		}
		metadataAttrService.batchCreateMetadataAttr(attrList);
		countService.updateAllParentCount(countManageModel);
		
		// create icon
		folderService.resetGlobalIconByUser(user.getId());
		// create backup folders
		backupMetadataService.registerRootBackup(backupRoot);
		backupMetadataService.backupMetadata(folders, user.getId());
		
		// TODO activityHistoy
		return userDao.getUser(user.getId());

	}

	private Metadata buildUserMetadata(String metadataId, String parentId,
			MetadataType type, String name, String originName, Long size,
			String userId, int sortPriority, boolean isVisible) {
		long nowUTCTime = DateUtils.nowUTCTimestamp();
		Metadata metadataObj = new Metadata();
		metadataObj.setId(metadataId);
		metadataObj.setParentId(parentId);
		metadataObj.setName(name);
		metadataObj.setFolder(true);
		metadataObj.setSortPriority(sortPriority);
		metadataObj.setTotalSize(size);
		metadataObj.setOriginName(originName);
		metadataObj.setModifiedAt(nowUTCTime);
		metadataObj.setModifiedBy(userId);
		metadataObj.setBackupCount("".equals(parentId) ? 1 : -1);
		metadataObj.setDeviceUid("");
		metadataObj.setFullSourcePath("");
		metadataObj.setNote("");
		metadataObj.setParams("");
		metadataObj.setIconBlockId("");
		metadataObj.setEncrypted(false);
		metadataObj.setCreatedAt(nowUTCTime);
		metadataObj.setCreatedBy(userId);
		metadataObj.setBlockId("");
		metadataObj.setType(type.toString());
		metadataObj.setSharedRootId("");
		metadataObj.setOwnerId(userId);
		metadataObj.setVisible(isVisible);
		return metadataObj;
	}

	private String getDeviceUID(GetUserRequest getUserRequest) {
		String deviceId = getUserRequest.getDeviceId();
		if (!StringUtils.isNullOrEmpty(deviceId)) {
			Device device = deviceDao.getDeviceByDeviceId(getUserRequest.getUserId(), deviceId);
			if (device == null) {
				// create devcie
				GetDeviceRequest getDeviceRequest = new GetDeviceRequest();
				getDeviceRequest.setDeviceId(deviceId);
				getDeviceRequest.setToken(getUserRequest.getToken());
				GetDeviceResponse getDeviceResponse = deviceService
						.createDevice(getUserRequest);
				return getDeviceResponse.getDeviceUid();
			}
			return device.getDeviceUid();
		}
		return "";
	}

	private List<ActiveHistory> convertActiveHistory(List<Metadata> metadatas,
			String token, EactivityHistoryType type, String DeviceUid) {
		List<ActiveHistory> list = new ArrayList<ActiveHistory>();
		for (Metadata item : metadatas) {
			ActiveHistory history = new ActiveHistory();
			history.setId(UUID.randomUUID().toString());
			history.setOldPath(" ");
			history.setMetadataId(item.getId());
			history.setLastEditUser(item.getOwnerId());
			history.setLastEditTime(DateUtils.nowUTCDateTime());
			history.setLevel(ActivityHistoryLevel.SYSTEM.toString());
			list.add(history);
			if (type.equals(EactivityHistoryType.RESET_USER)) {
				history.setPath(item.getOwnerId() + "/");
				history.setChangeReason(SystemEvent.RESET_USER.toString());
			}
			if (type.equals(EactivityHistoryType.RESET_COMMON)) {
				history.setPath(item.getOwnerId() + "/");
				history.setChangeReason(SystemEvent.RESET_COMMON.toString());
			}
			if (type.equals(EactivityHistoryType.RESET_COMMON_ALL)) {
				history.setPath(item.getOwnerId() + "/my device sync folders/"
						+ DeviceUid);
				history.setChangeReason(SystemEvent.RESET_COMMON.toString());
			}
		}
		return list;
	}

	private List<Metadata> filterRestTarget(List<Metadata> metas,
			String deviceUid) {
		List<Metadata> list = new ArrayList<Metadata>();
		if (StringUtils.isNullOrEmpty(deviceUid)) {
			return metas;
		}
		for (Metadata item : metas) {
			if (item.getDeviceUid().equals(deviceUid)) {
				list.add(item);
			}
		}
		return list;
	}

	private void deleteMobileFolder(String userId, String deviceUid) {
		if (StringUtils.isNullOrEmpty(deviceUid)) {
			userDao.deleteAllMobileFolders(userId);// delete
		} else {
			userDao.deleteMobileFolders(userId, deviceUid);// delete
		}
	}

	private List<String> getIdList(List<? extends BaseMetadata> metadatas) {
		List<String> list = new ArrayList<String>();
		for (BaseMetadata item : metadatas) {
			list.add(item.getId());
		}
		return list;
	}

	private String getPathName(String fullPath) {
		List<String> path = Arrays.asList(fullPath.split("/"));
		if (path.get(path.size() - 1).lastIndexOf(".") < 1 || path.size() == 1) {
			return path.get(path.size() - 1);
		}
		return path.get(path.size() - 2);

	}

}
