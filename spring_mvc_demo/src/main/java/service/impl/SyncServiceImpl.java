package service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import model.ActiveHistory;
import model.CellPhone;
import model.CollaborateMember;
import model.GlobalSyncApp;
import model.Metadata;
import model.MetadataAttr;
import model.PathInfo;
import model.RequestBase;
import model.request.AddGlobalAppRequest;
import model.request.DeleteGlobalAppRequest;
import model.request.DeleteUserAppRequest;
import model.request.GetActivityHistoryRequest;
import model.request.GetUserAppRequest;
import model.request.SwitchAppSyncableRequest;
import model.request.UpdateUserAppRequest;
import model.response.GetActivityHistoryResponse;
import model.response.GetGlobalAppResponse;
import model.response.GetUserAppResponse;

import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import service.BackupMetadataService;
import service.CountService;
import service.DynamoDBService;
import service.MetadataAttrService;
import service.SSOService;
import service.SyncService;
import utils.CellphonesUtils;
import utils.DateUtils;
import utils.StringUtils;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

import constants.CommonFolders;
import constants.HttpStatus;
import constants.MetadataType;
import dao.CollaborateDao;
import dao.MetadataDao;
import dao.SyncAppDao;
import exception.MetadataException;
import factory.MetadataFactory;
import factory.PathFactory;

@Service
public class SyncServiceImpl implements SyncService {

	@Autowired
	private SyncAppDao syncAppDao;
	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private CollaborateDao collaborateDao;
	@Autowired
	private MetadataFactory metadataFactory;
	@Autowired
	private BackupMetadataService backupMetadataService;
	@Autowired
	private CountService countService;
	@Autowired
	private MetadataAttrService metadataAttrService;
	@Autowired
	private PathFactory pathFactory;
	@Autowired
	private DynamoDBService dynamoDBService;
	@Autowired
	private SSOService ssoService;
	
	@Override
	public void addGlobalApp(AddGlobalAppRequest addGlobalAppRequest)
			throws MetadataException {
		List<GlobalSyncApp> apps = syncAppDao.getGlobalAppByAppName(addGlobalAppRequest.getAppName());
		if (apps != null && !apps.isEmpty()){
			throw new MetadataException(HttpStatus.FILE_EXIST);
		}
		List<GlobalSyncApp> globalApps = new ArrayList<GlobalSyncApp>();
		for(String folderPath : addGlobalAppRequest.getFolderPath()){
			GlobalSyncApp globalApp = new GlobalSyncApp();
			globalApp.setAppName(addGlobalAppRequest.getAppName());
			globalApp.setSingleFolderPath(folderPath);
			globalApps.add(globalApp);
		}
		syncAppDao.addGlobalApp(globalApps);
	}

	@Override
	public void deleteGlobalApp(DeleteGlobalAppRequest deleteGlobalAppRequest)
			throws MetadataException {
		List<GlobalSyncApp> apps = syncAppDao.getGlobalAppByAppName(deleteGlobalAppRequest.getAppName());
		if (apps.isEmpty()){
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		syncAppDao.deleteGlobalApp(deleteGlobalAppRequest.getAppName());
	}

	@Override
	public List<GetGlobalAppResponse> getGlobalApp(
			RequestBase getGlobalAppRequest) throws MetadataException {
		List<GlobalSyncApp> globalApps = syncAppDao.getGlobalApp();
		List<GetGlobalAppResponse> result = new ArrayList<GetGlobalAppResponse>();
		for(GlobalSyncApp globalApp : globalApps){
			result.add(globalApp.toGetGlobalAppResponse());
		}
		return result;
	}

	@Override
	public void updateUserApp(UpdateUserAppRequest updateUserAppRequest)
			throws MetadataException {
		List<GlobalSyncApp> userGlobalSyncApps = syncAppDao.getUserDefinedAppByUser(updateUserAppRequest.getUserId());
		List<GlobalSyncApp> addGlobalSyncApps = new ArrayList<GlobalSyncApp>();
		List<GlobalSyncApp> updateGlobalSyncApps = new ArrayList<GlobalSyncApp>();
		for (String appName : updateUserAppRequest.getAppName()){
			boolean find = false;
			if (userGlobalSyncApps != null && !userGlobalSyncApps.isEmpty()){
				for (GlobalSyncApp globalSyncApp : userGlobalSyncApps){
					if (appName.equals(globalSyncApp.getAppName()) && 
						updateUserAppRequest.getDeviceUniqueId().equals(globalSyncApp.getDeviceUniqueId())){
						if (globalSyncApp.isUninstalled()){
							globalSyncApp.setUninstalled(false);
							globalSyncApp.setSyncable(false);
							updateGlobalSyncApps.add(globalSyncApp);
						}
						find = true;
						break;
					}
				}
		    }
			if (!find){
				GlobalSyncApp addglobalSyncApp = new GlobalSyncApp();
				addglobalSyncApp.setUser(updateUserAppRequest.getUserId());
				addglobalSyncApp.setDeviceUniqueId(updateUserAppRequest.getDeviceUniqueId());
				addglobalSyncApp.setAppName(appName);
				addglobalSyncApp.setSyncable(false);
				addglobalSyncApp.setUninstalled(false);
				addGlobalSyncApps.add(addglobalSyncApp);
			}
		}
		for (GlobalSyncApp globalSyncApp : userGlobalSyncApps){
			boolean findApp = false;
			for (String appName : updateUserAppRequest.getAppName()){
				if (appName.equals(globalSyncApp.getAppName())){
					findApp = true;
					break;
				}
			}
			if (!findApp){
				GlobalSyncApp updateGlobalSyncApp = globalSyncApp;
				updateGlobalSyncApp.setDeviceUniqueId(updateUserAppRequest.getDeviceUniqueId());
				updateGlobalSyncApp.setSyncable(false);
				updateGlobalSyncApp.setUninstalled(true);
				updateGlobalSyncApps.add(updateGlobalSyncApp);
			}
		}
		syncAppDao.addUserDefinedApp(addGlobalSyncApps);
		syncAppDao.updateUserDefinedApp(updateGlobalSyncApps);
	}

	@Override
	public List<GetUserAppResponse> getUserApp(
			GetUserAppRequest getUserAppRequest) throws MetadataException {
		List<GlobalSyncApp> globalSyncApps =
				syncAppDao.getUserDefinedAppByIntersection(getUserAppRequest.getUserId(),
			    getUserAppRequest.getDeviceUniqueId());
		List<GetUserAppResponse> result = new ArrayList<GetUserAppResponse>();
		String tempAppName = "";
		String tempDeviceUnuqueId = "";
		boolean tempIsSyncable = false;
		boolean tempIsUninstall = false;
		GetUserAppResponse res = null;
		List<String> folderPath = null;
		for(GlobalSyncApp globalSyncApp : globalSyncApps){
			if (!tempAppName.equals(globalSyncApp.getAppName()) || 
				!tempDeviceUnuqueId.equals(globalSyncApp.getDeviceUniqueId())){
				if (res != null){
					res.setFolderPath(folderPath);
					result.add(res);
				}
				res = new GetUserAppResponse();
				res.setAppName(globalSyncApp.getAppName());
				res.setDeviceUniqueId(globalSyncApp.getDeviceUniqueId());
				res.setSourcePath(CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString().concat("/").concat(globalSyncApp.getDeviceUniqueId()));
				res.setPath(CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString().concat("/").concat(globalSyncApp.getAppName()));
				res.setSyncable(globalSyncApp.isSyncable());
				res.setUninstalled(globalSyncApp.isUninstalled());
				tempAppName = globalSyncApp.getAppName();
				tempDeviceUnuqueId = globalSyncApp.getDeviceUniqueId();
				tempIsSyncable = globalSyncApp.isSyncable();
				tempIsUninstall = globalSyncApp.isUninstalled();
				folderPath = new ArrayList<String>();
			}
			folderPath.add(globalSyncApp.getSingleFolderPath());
		}
		//last
		res = new GetUserAppResponse();
		res.setAppName(tempAppName);
		res.setDeviceUniqueId(tempDeviceUnuqueId);
		res.setSourcePath(CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString().concat("/").concat(tempDeviceUnuqueId));
		res.setPath(CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString().concat("/").concat(tempAppName));
		res.setFolderPath(folderPath);
		res.setSyncable(tempIsSyncable);
		res.setUninstalled(tempIsUninstall);
		result.add(res);
		return result;
	}

	@Override
	@Transactional
	public void deleteUserApp(DeleteUserAppRequest deleteUserAppRequest)
			throws MetadataException {
		syncAppDao.deleteUserDefinedApp(deleteUserAppRequest.getUserId(),
				deleteUserAppRequest.getDeviceUniqueId(),deleteUserAppRequest.getAppName());
		String syncAppPath = deleteUserAppRequest.getUserId().concat("/").concat(CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString());
	    Metadata syncAppRootMetadata = metadataDao.getMetadataByPath(syncAppPath, "", "");
	    Metadata syncAppMetadata = metadataDao.getMetadataByCriteria(syncAppRootMetadata.getId(), 
	    		deleteUserAppRequest.getAppName().toLowerCase(), 
	    		deleteUserAppRequest.getDeviceUniqueId(), "");
		List<Metadata> metadatas = metadataFactory.getAllSubFoldersAndFiles(syncAppMetadata);
		metadatas.add(syncAppMetadata);
		for (Metadata metadata : metadatas){
			metadataDao.deleteMetadata(metadata.getId());
			metadataAttrService.deleteMetadataAttr(metadata.getId());	
			backupMetadataService.unbackupMetadata(metadata.getId());
		}
		List<MetadataAttr> metadataAttr = metadataFactory.generateMetadataAttrsForUpdateParents(
				syncAppMetadata, null, syncAppMetadata.getFoldersCount()+1, 
				-syncAppMetadata.getFilesCount(), -syncAppMetadata.getTotalSize());
		metadataAttrService.batchUpdateMetadataAttrOnOrigin(metadataAttr);
	}

	@Override
	@Transactional
	public void switchAppSyncable(
			SwitchAppSyncableRequest switchAppSyncableRequest)
			throws MetadataException {
		GlobalSyncApp userSyncApp = syncAppDao.getUserDefinedAppByKey(switchAppSyncableRequest.getUserId(),
				switchAppSyncableRequest.getDeviceUniqueId(),switchAppSyncableRequest.getAppName());
		if (userSyncApp == null){
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (userSyncApp.isSyncable() == switchAppSyncableRequest.isSyncable()){
			throw new MetadataException(HttpStatus.FILE_EXIST);
		}
		List<GlobalSyncApp> globalSyncApps = new ArrayList<GlobalSyncApp>();
		userSyncApp.setSyncable(switchAppSyncableRequest.isSyncable());
		globalSyncApps.add(userSyncApp);
		syncAppDao.updateUserDefinedApp(globalSyncApps);
		
		List<Metadata> createFolders = new ArrayList<Metadata>();
		if (switchAppSyncableRequest.isSyncable()){
			List<GlobalSyncApp> defaultFolders = syncAppDao.getGlobalAppByAppName(switchAppSyncableRequest.getAppName());
		    String syncAppRootPath = switchAppSyncableRequest.getUserId() +"/" + CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString();
		    Metadata syncAppRootMetadata = metadataDao.getMetadataByPath(syncAppRootPath, "", "");
		    Metadata syncAppMetadata = metadataDao.getMetadataByCriteria(syncAppRootMetadata.getId(), 
		    	switchAppSyncableRequest.getAppName().toLowerCase(), 
		    	switchAppSyncableRequest.getDeviceUniqueId(), "");
		    if (syncAppMetadata != null){
		    	List<Metadata> subFolders = metadataDao.getMetadatasByParentId(syncAppMetadata.getId());
		    	for(GlobalSyncApp defaultFolder : defaultFolders){
		    		boolean findDefault = false;
		    		String fullAppPath = syncAppRootPath + "/" + 
		    		        switchAppSyncableRequest.getDeviceUniqueId() +"/" + 
		    			    defaultFolder.getSingleFolderPath();
		    		if (subFolders != null && !subFolders.isEmpty()){
			    		for(Metadata subFolder : subFolders){
			    			if (fullAppPath.equals(subFolder.getFullSourcePath())){
			    				findDefault = true;
			    			}
			    		}
		    		}
		    		if (!findDefault){
		    			Metadata createFolder = buildSyncAppFolderMetadata(defaultFolder.getSingleFolderPath(),
		    					syncAppMetadata.getId(),switchAppSyncableRequest.getDeviceUniqueId(),
		    					switchAppSyncableRequest.getUserId(),fullAppPath);
		    			createFolders.add(createFolder);
		    		}
		    	}
		    }else{
		    	syncAppMetadata = buildSyncAppFolderMetadata(switchAppSyncableRequest.getAppName(),
		    			syncAppRootMetadata.getId(),switchAppSyncableRequest.getDeviceUniqueId(),
    					switchAppSyncableRequest.getUserId(),"");
		    	createFolders.add(syncAppMetadata);
		    	for(GlobalSyncApp defaultFolder : defaultFolders){
		    		String fullAppPath = syncAppRootPath + "/" + 
		    		        switchAppSyncableRequest.getDeviceUniqueId() + "/" + 
		    			    defaultFolder.getSingleFolderPath();
			    	Metadata createFolder = buildSyncAppFolderMetadata(FilenameUtils.getName(defaultFolder.getSingleFolderPath()),
	    					syncAppMetadata.getId(),switchAppSyncableRequest.getDeviceUniqueId(),
	    					switchAppSyncableRequest.getUserId(),fullAppPath);
	    			createFolders.add(createFolder);
		    	}
		    }
		}
		
		for(Metadata createFolder : createFolders){
			createFolder.setSortPriority(metadataDao.getMaxSortPriority(createFolder.getParentId())+1);
			metadataDao.createMetadata(createFolder);
			//backupMetadataService.backupMetadata(createFolder, switchAppSyncableRequest.getUserId(), true);
			backupMetadataService.createSingleBackup(createFolder, switchAppSyncableRequest.getUserId());
			countService.recalculateForCreatingFolders(Arrays
					.asList(createFolder));
		}
	}
	
	private Metadata buildSyncAppFolderMetadata(String folderName,String parentId,
			String deviceId,String userId,String fullSourcePath) {
		long nowUTCTime = DateUtils.nowUTCTimestamp();
		String metadataIndexId = StringUtils.getUUID();
		Metadata metadataObj = new Metadata();
		metadataObj.setId(metadataIndexId);
		metadataObj.setParentId(parentId);
		metadataObj.setOriginName(folderName);
		metadataObj.setName(folderName.toLowerCase());
		metadataObj.setFolder(true);
		metadataObj.setModifiedAt(nowUTCTime);
		metadataObj.setModifiedBy(userId);
		metadataObj.setCreatedAt(nowUTCTime);
		metadataObj.setCreatedBy(userId);
		metadataObj.setType(MetadataType.COMMON.toString());
		metadataObj.setOwnerId(userId);
		metadataObj.setDeviceUid(deviceId);
		metadataObj.setFullSourcePath(fullSourcePath);
		return metadataObj;
	}

	@Override
	public GetActivityHistoryResponse searchHistory(
			GetActivityHistoryRequest getActivityHistoryRequest)
			throws MetadataException {	

		
		List<ActiveHistory> allHistories = new ArrayList<ActiveHistory>();
		String searchKey = getActivityHistoryRequest.getSearchKey();
		List<String> sharedRootIds = new ArrayList<String>();
		// if path = "my shared folder"
		if (getActivityHistoryRequest.getPath().toLowerCase().equals(CommonFolders.MY_SHARED_FOLDERS.toString())) {
			// user = owner
			List<Metadata> selfShared = metadataDao.getMetadatasByType(getActivityHistoryRequest.getUserId(), MetadataType.SHARE);
			List<CellPhone> inquirerCellphones = dynamoDBService.findCellphonesByUserId(getActivityHistoryRequest.getUserId());
			String cellphones = CellphonesUtils.composeCellphones(inquirerCellphones);
			List<CollaborateMember> members = collaborateDao.getCollaborateMembersByCellphones(cellphones, false, false, false);
			
			for (Metadata metadata : selfShared) {
				// get user = owner shared folder 
				sharedRootIds.add(metadata.getSharedRootId());
			}
			for (CollaborateMember member : members) {
				if (member.getAccepted() != null && member.getAccepted()) {
					// member is accepted
					sharedRootIds.add(member.getMetadataIndexId());
				}			
			}
			allHistories = dynamoDBService.getActiveHistory(sharedRootIds,
																											getActivityHistoryRequest.getToDate(),
																											"",
																											true);
		}else{
			PathInfo pathInfo = pathFactory.parsePathInfo(getActivityHistoryRequest, false, true);
			Metadata metadata = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), "", "");
			if (metadata == null) {
				throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
			}
			sharedRootIds.add(metadata.getSharedRootId());
			allHistories = dynamoDBService.getActiveHistory(sharedRootIds,
																											getActivityHistoryRequest.getToDate(),
																											pathInfo.getFullOwnerPath(),
																											metadata.isFolder());
		}
		
		List<ActiveHistory> resultsList  = new  LinkedList<ActiveHistory>();
		
		if (StringUtils.isNullOrEmpty(searchKey)) {
			resultsList.addAll(allHistories);
			allHistories = null;
		}else {
			List<ActiveHistory> tempHistories = new ArrayList<ActiveHistory>();
			// search key compare path,last_edit_time,change_reason
			for (ActiveHistory activeHistory : allHistories) {
				if (activeHistory.getPath().contains(searchKey)
					 || activeHistory.getLastEditTime().contains(searchKey)
					 || activeHistory.getChangeReason().equals(searchKey)) {
					resultsList.add(activeHistory);
				}else {
					tempHistories.add(activeHistory);
				}
			}
			// search key compare with note
			resultsList.addAll(searchNote(tempHistories,getActivityHistoryRequest.getSearchKey()));
		}
		Collections.sort(resultsList);
		Collections.reverse(resultsList);
		// relative path
		Map<String, String>relativePathMap = relativePath(sharedRootIds,getActivityHistoryRequest.getUserId());
		// group by date
		Map<String ,List<ActiveHistory>> dateMap = groupByDate(resultsList);
		Set<String> dateSet = dateMap.keySet();
		//count period
		int period = dateSet.size() - getActivityHistoryRequest.getPeriod();
		int showPeriod = period > 0 ? getActivityHistoryRequest.getPeriod():dateSet.size();
		//group by user
		LinkedHashMap<String, Map<String ,List<ActiveHistory>>> linkedHashMap = new LinkedHashMap<String, Map<String,List<ActiveHistory>>>();
		int i = 0;
		for (String date : dateSet) {
			if (i >=showPeriod) {
				break;
			}
			Map<String ,List<ActiveHistory>> userMap = groupByUser(dateMap.get(date),relativePathMap,getActivityHistoryRequest.getUserId());
			linkedHashMap.put(date, userMap);
			i++;
		}
		// response
		GetActivityHistoryResponse getActivityHistoryResponse = new GetActivityHistoryResponse();
		getActivityHistoryResponse.setActiveHistories(linkedHashMap);
		getActivityHistoryResponse.setLastPeriods(period>0?period:0);
		getActivityHistoryResponse.setLastEditTime(resultsList.size()>0?resultsList.get(0).getLastEditTime():"");
		return getActivityHistoryResponse;
	}
	
	private Map<String ,List<ActiveHistory>>  groupByDate(List<ActiveHistory> activeHistories){
		Map<String ,List<ActiveHistory>> map = new HashMap<String, List<ActiveHistory>>();
		for (ActiveHistory activeHistory : activeHistories) {
			String date = DateUtils.trimToDate(activeHistory.getLastEditTime(), "yyyy-MM-dd");
			
			if (map.containsKey(date)) {
				map.get(date).add(activeHistory);
			}else {
				List<ActiveHistory> listHistorys = new LinkedList<ActiveHistory>();
				listHistorys.add(activeHistory);
				if (date == null) {
					date = "";
				}
				map.put(date, listHistorys);
			}
		}
		Map<String, List<ActiveHistory>> reverseMap = new TreeMap<String, List<ActiveHistory>>(Collections.reverseOrder());
		reverseMap.putAll(map);
		return reverseMap;		
	}
	private Map<String ,List<ActiveHistory>> groupByUser(List<ActiveHistory> activeHistories,Map<String, String>relativePathMap,String currentUserId){
		Map<String ,List<ActiveHistory>> map = new TreeMap<String, List<ActiveHistory>>();
		for (ActiveHistory activeHistory : activeHistories) {
			String user = activeHistory.getLastEditUser();
			String sharedUpperPath = activeHistory.getSharedUpperPath();
			String path = activeHistory.getPath();
			String ownerId = path.split("/")[0];
			
			if (!ownerId.equals(currentUserId)) {
				String relativePath = relativePathMap.get(activeHistory.getSharedRootId());				
				path = path.replace(sharedUpperPath, relativePath);	
			}else{
				path = StringUtils.trimFilePathRoot(path);
			}
			activeHistory.setPath(path);
			if (map.containsKey(user)) {
				String userDisplayName = map.get(user).get(0).getLastEditUserName();
				String photoBlockId = map.get(user).get(0).getPhotoBlockId();
				activeHistory.setLastEditUserName(userDisplayName);
				activeHistory.setPhotoBlockId(photoBlockId);				
				map.get(user).add(activeHistory);
			}else {
				String userDisplayName = ssoService.findDisplayNameByUserId(activeHistory.getLastEditUser());
				List<CellPhone>cellPhones = dynamoDBService.findCellphonesByUserId(activeHistory.getLastEditUser());
				CollaborateMember member = collaborateDao.getCollaborateMemberByFragmentCellphone(
						activeHistory.getSharedRootId(),
						CellphonesUtils.composeCellphones(cellPhones));
				List<ActiveHistory> listHistorys = new ArrayList<ActiveHistory>();
				activeHistory.setLastEditUserName(userDisplayName);
				if (member != null) {
					activeHistory.setPhotoBlockId(member.getPhotoBlockId());
				}				
				listHistorys.add(activeHistory);
				map.put(user, listHistorys);
			}
			activeHistory.setId(null);
			activeHistory.setOldPath(null);
			activeHistory.setSharedRootId(null);
			activeHistory.setMetadataId(null);
			activeHistory.setLevel(null);
			activeHistory.setSharedUpperPath(null);
			activeHistory.setOldSharedUpperPath(null);
		}
		return map;
	}
	private List<ActiveHistory> searchNote(List<ActiveHistory> activeHistories,String searchKey){
		//group by shared root id
		Multimap<String, ActiveHistory> groups = ArrayListMultimap.create();  
	    for (ActiveHistory activeHistory : activeHistories) {  
	        groups.put(activeHistory.getSharedRootId(), activeHistory);  
	    }
	    
	    List<Metadata> metadataIds = new ArrayList<Metadata>();
		for (String sharedRootId : groups.keySet()) {
			metadataIds.addAll(metadataDao.getMetadataBySharedRootId(sharedRootId, searchKey));
		}
		// compare table "activity_history" metadata_indexid with table "metadata_index"  id
		List<ActiveHistory> reasults = new ArrayList<ActiveHistory>();
		for (Metadata metadata : metadataIds) {
			for (ActiveHistory acaHistory : activeHistories) {
				if (acaHistory.getMetadataId() != null && acaHistory.getMetadataId().equals(metadata.getId())) {
					reasults.add(acaHistory);
				}
			}
		}
		return reasults;
	}
	private Map<String, String> relativePath(List<String> sharedIds,String viewerId){
		Map<String,String> pathMap = new HashMap<String, String>(); 
		for (String sharedId : sharedIds) {
			if (pathMap.containsKey(sharedId)) {
				continue;
			}
			String shareRelativcePath = metadataFactory.getMetadataRelativePath(sharedId, viewerId);
			pathMap.put(sharedId, shareRelativcePath);
		}
		return pathMap;
	}
}
