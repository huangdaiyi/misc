package service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import model.BaseMetadata;
import model.CountManageModel;
import model.Metadata;
import model.PathInfo;
import model.PathRequestBase;
import model.RequestBase;
import model.SyncRelation;
import model.request.SyncRelationPathRequest;
import model.request.SyncRelationRequest;
import model.response.SyncRelationResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import service.CountService;
import service.DynamoDBService;
import service.FileService;
import service.FolderService;
import service.SyncRelationService;
import service.XmppService;
import utils.DateUtils;
import utils.StringUtils;
import constants.ActivityHistoryLevel;
import constants.ChangeReason;
import constants.HttpStatus;
import constants.SyncRelationAction;
import constants.SystemEvent;
import dao.MetadataDao;
import dao.SyncRelationDao;
import exception.MetadataException;
import factory.MetadataFactory;
import factory.PathFactory;

@Service
public class SyncRelationServiceImpl implements SyncRelationService {

	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private SyncRelationDao syncRelationDao;
	@Autowired
	private FolderService folderService;
	@Autowired
	private FileService fileService;
	@Autowired
	private MetadataFactory metadataFactory;
	@Autowired
	private PathFactory pathFactory;
	@Autowired
	private DynamoDBService dynamoDBService;
	@Autowired
	private XmppService xmppService;
	@Autowired
	private CountService countService;
	
	// Public
	
	@Override
	public void addSyncRelation(SyncRelationRequest syncRelationRequest)
			throws MetadataException {
		addSyncRelation(syncRelationRequest, false);
	}
	
	@Override
	public List<SyncRelationResponse> querySyncRelation(SyncRelationPathRequest syncRelationPathRequest)
			throws MetadataException {
		
		List<SyncRelation> syncRelations = new ArrayList<SyncRelation>();
		Map<String, SyncRelation> map = new HashMap<String, SyncRelation>();
		Map<String, Metadata> cachedMetadata = new HashMap<String, Metadata>();
		PathInfo pathInfo = pathFactory.parsePathInfo(syncRelationPathRequest, true, true);
		
		if(pathInfo.isUnderMyDeviceSyncFolders())
		{
			addSyncRelationsToMap(map, syncRelationDao.getLowerLevelFoldersOfClient(syncRelationPathRequest.getUserId(), syncRelationPathRequest.getPath(), true));
		}
		else
		{
			Metadata queryRoot = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), "", "");
			addMetadataToCache(cachedMetadata, queryRoot);
			
			if(queryRoot != null) {
				List<Metadata> folders = new ArrayList<Metadata>();
				
				if(queryRoot.getId().equals(queryRoot.getSyncRootId())) {
					folders.add(queryRoot);
				}
				
				folders.addAll(metadataFactory.getAllSubFolders(queryRoot));
				addMetadataToCache(cachedMetadata, folders);
				
				for(Metadata folder : folders) {
					if(folder.getId().equals(folder.getSyncRootId())) {
						addSyncRelationsToMap(map, getSyncRelationsByMetadataId(folder.getId()));
					}
				}
			}
		}
		
		syncRelations.addAll(map.values());
		
		List<SyncRelationResponse> result = new ArrayList<SyncRelationResponse>();
		for(SyncRelation syncRelation : distinctSyncRelations(syncRelations)) {
			SyncRelationResponse response = new SyncRelationResponse();
			
			// let local path be the source
			if(syncRelation.isDestinationInClient()) {
				syncRelation.swapSourceTarget();
			}
						
			response.setOwnerId(syncRelation.getOwnerId());
			if(syncRelation.isSourceInClient()) {
				response.setPath(syncRelation.getSourcePath());
			} else {
				Metadata folder = null;
				if(cachedMetadata.containsKey(syncRelation.getMetadataIndexId())) {
					folder = cachedMetadata.get(syncRelation.getMetadataIndexId());
				}
				else {
					folder = metadataDao.getMetadata(syncRelation.getMetadataIndexId());
				}
				response.setPath(metadataFactory.getMetadataOriginalPath(folder));
			}
			
			response.setDestinationOwnerId(syncRelation.getDestinationOwnerId());
			if(syncRelation.isDestinationInClient()) {
				response.setDestinationPath(syncRelation.getDestinationSourcePath());
			} else {
				Metadata folder = null;
				if(cachedMetadata.containsKey(syncRelation.getDestinationMetadataIndexId())) {
					folder = cachedMetadata.get(syncRelation.getDestinationMetadataIndexId());
				}
				else {
					folder = metadataDao.getMetadata(syncRelation.getDestinationMetadataIndexId());
				}
				response.setDestinationPath(metadataFactory.getMetadataOriginalPath(folder));
			}
			
			response.setAction(syncRelation.getAction().toString());
			response.setEnable(syncRelation.getEnable());
			
			result.add(response);
		}
		
		return result;
	}
	
	@Override
	public void updateSyncRelation(SyncRelationRequest syncRelationRequest)
			throws MetadataException {
		
		SyncRelation criteria = parseRequest(syncRelationRequest);
		criteria.setEnable(null);
		
		List<SyncRelation> syncRelations = distinctSyncRelations(syncRelationDao.getSyncRelationByCriteria(criteria));
		
		if(syncRelations.size() == 0) {
			throw new MetadataException(HttpStatus.RELATION_NOT_FOUND);
		}
		
		for(SyncRelation syncRelation : syncRelations) {
			syncRelationDao.updateSyncRelationEnable(syncRelation.getId(), syncRelationRequest.isEnable());
			
			if(syncRelation.getAction() == SyncRelationAction.SYNC) {
				SyncRelation swappedSyncRelation = syncRelation.createSwappedSyncRelation();
				swappedSyncRelation.setId(0);
				swappedSyncRelation.setEnable(null);
				List<SyncRelation> syncs = syncRelationDao.getSyncRelationByCriteria(swappedSyncRelation);
				for(SyncRelation sync : syncs) {
					syncRelationDao.updateSyncRelationEnable(sync.getId(), syncRelationRequest.isEnable());
				}
			}
			
			if(!syncRelationRequest.isEnable()) {
				cleanSynRelationRelatedFiles(syncRelation);
				cleanSynRelationRelatedEmptyFolders(syncRelationRequest.getUserId(), syncRelation);
			}
		}
		
		sendXmppMessage(syncRelationRequest, SystemEvent.UPDATE_SYNC_RELATION, syncRelations);
		addActivityHistory(criteria, syncRelationRequest.getUserId(), ChangeReason.UPDATE_SYNC_RELATION);
	}
	
	@Override
	public void deleteSyncRelations(String userId, List<SyncRelation> syncRelations) {
		syncRelations = distinctSyncRelations(syncRelations);
		
		if(syncRelations.size() == 0) {
			throw new MetadataException(HttpStatus.RELATION_NOT_FOUND);
		}
		
		for(SyncRelation syncRelation : syncRelations) {
			try {
				Metadata metadataForXmpp = null;
				CountManageModel countManageModel = new CountManageModel();
				
				// set the files to normal, but reserve the sync_root_id
				
				if(!syncRelation.isSourceInClient()) {
					Metadata source = metadataDao.getMetadata(syncRelation.getMetadataIndexId());
					if(source != null) {
						for(Metadata metadata : metadataFactory.getAllSubFoldersAndFiles(source)) {
							metadataDao.transformSyncToNormal(metadata.getId(), true);
						}
						metadataForXmpp = source;
						countManageModel.addFolder(source.getId(), 0, 0, 0, 0, -1, 0);
					}
				}
				
				if(!syncRelation.isDestinationInClient()) {
					Metadata target = metadataDao.getMetadata(syncRelation.getDestinationMetadataIndexId());
					if(target != null) {
						for(Metadata metadata : metadataFactory.getAllSubFoldersAndFiles(target)) {
							metadataDao.transformSyncToNormal(metadata.getId(), true);
						}
						metadataForXmpp = target;
						countManageModel.addFolder(target.getId(), 0, 0, 0, 0, -1, 0);
					}
				}
				
				countService.updateAllParentCount(countManageModel);
				
				syncRelation.setEnable(null); // ignore the 'enable' value
				syncRelationDao.deleteSyncRelation(syncRelation);
				
				if(metadataForXmpp != null) {
					sendXmppMessage(metadataForXmpp, SystemEvent.DELETE_SYNC_RELATION, userId);
				}
				
				if(syncRelation.getAction() == SyncRelationAction.SYNC) {
					SyncRelation swappedSyncRelation = syncRelation.createSwappedSyncRelation();
					swappedSyncRelation.setId(0);
					swappedSyncRelation.setEnable(null);
					syncRelationDao.deleteSyncRelation(swappedSyncRelation);
				}
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	@Override
	public void deleteSyncRelation(SyncRelationRequest syncRelationRequest)
			throws MetadataException {
		SyncRelation syncRelation = parseRequest(syncRelationRequest);
		deleteSyncRelations(syncRelationRequest.getUserId(), Arrays.asList(syncRelation));
		
		transformToNormalIfNoRelationExists(syncRelation);
		
		addActivityHistory(syncRelation, syncRelationRequest.getUserId(), ChangeReason.DELETE_SYNC_RELATION);
	}
	
	@Override
	public void deleteRelatedSyncRelation(String syncRootId, String userId)
			throws MetadataException {
		
		// delete both of source and target which is the metadata
		SyncRelation[] criterias = {new SyncRelation(), new SyncRelation()};
		criterias[0].setMetadataIndexId(syncRootId);
		criterias[1].setDestinationMetadataIndexId(syncRootId);
		
		List<SyncRelation> syncRelations = new ArrayList<SyncRelation>();
		
		for(SyncRelation criteria : criterias) {
			try {
				syncRelations.addAll(syncRelationDao.getSyncRelationByCriteria(criteria));
			} catch (MetadataException e) {
				if(e.getHttpStatus() != HttpStatus.RELATION_NOT_FOUND) {
					throw e;
				}
			}
		}

		deleteSyncRelations(userId, syncRelations);
		transformToNormalIfNoRelationExists(syncRootId);
	}
	
	@Override
	public void sync(BaseMetadata metadata, SystemEvent operation)
			throws MetadataException{
		sync(null, metadata, operation);
	}
	
	@Override
	public void sync(RequestBase request, BaseMetadata metadata, SystemEvent operation)
			throws MetadataException {
		
		// do nothing if no any relation exists
		if(StringUtils.isNullOrEmpty(metadata.getSyncRootId())) return;
		
		Set<String> visited = new HashSet<String>();
		Queue<SyncRelation> queue = new LinkedList<SyncRelation>();
		queue.addAll(getSyncRelationBySourceMetadataIndexId(metadata.getSyncRootId()));
		
		// List<String> splittedRelativePath = getRelativePathUnderSyncRoot(metadata);
		
		SyncRelation syncRelation = null;
		while((syncRelation = queue.poll()) != null) {
			// do nothing if target folder is in client
			if(syncRelation.isDestinationInClient()) continue;
			
			String key = syncRelation.toRelationKey();
			
			// Same relation has been executed, ignore the relation.
			// This can avoid the auto sync relation to be executed in cycle.
			if(visited.contains(key)) continue;
			
			switch(operation) {
				// files
				case CREATE_FILE:
				case COPY_FILE:
				case MOVE_FILE:
				case DELETE_FILE:
				case RENAME_FILE:
				case UPDATE_FILE:
				case UPDATE_IMAGE:
				
				// folders
				case CREATE_FOLDER:
				case COPY_FOLDER:
				case MOVE_FOLDER:
				case DELETE_FOLDER:
				case RENAME_FOLDER:
				case SET_PROFILE_PROPERTY:
				case DELETE_PROFILE_PROPERTY:
				
				// both files and folders
				case SET_ICON:
				case UPDATE_NOTE:
				case UPDATE_FILE_PROCESSING_STATUS:
				
				// unimplemented
				case UPDATE_FOLDER: break;
				default:
					throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
			}
			
			visited.add(syncRelation.toRelationKey());
		}
		
	}

	@Override
	public void transformNormalToSync(SyncRelationRequest syncRelationRequest)
			throws MetadataException {
		SyncRelation syncRelation = addSyncRelation(syncRelationRequest, true);
		
		sendXmppMessage(syncRelationRequest, SystemEvent.TRANSFORM_NORMAL_TO_SYNC, syncRelation.getDestinationOrSourceSyncRootId());
		addActivityHistory(syncRelation, syncRelationRequest.getUserId(), ChangeReason.TRANSFORM_NORMAL_TO_SYNC);
	}

	@Override
	public void transformSyncToNormal(PathRequestBase pathRequest)
			throws MetadataException {
		BaseMetadata folder = metadataFactory.getFolder(pathRequest.getUserId(), pathRequest.getPath(), pathRequest.getOwnerId(), pathRequest.getSharedRootId());
		
		if(folder == null) return;
		
		if(!folder.getId().equals(folder.getSyncRootId())) {
			throw new MetadataException(HttpStatus.FOLDER_IS_NOT_A_SYNC_ROOT);
		}
		
		deleteRelatedSyncRelation(folder.getId(), pathRequest.getUserId());
		transformSyncToNormal(folder);

		sendXmppMessage(pathRequest, SystemEvent.TRANSFORM_SYNC_TO_NORMAL, folder.getSyncRootId());
		addActivityHistory(folder.getId(), pathRequest.getUserId(), ChangeReason.TRANSFORM_SYNC_TO_NORMAL);
	}
	
	/**
	 * check the callee is called by this class
	 */
	@Override
	public boolean isCaller() {
//		Throwable throwable = new Throwable();
//		StackTraceElement[] stackTrace = throwable.getStackTrace();
//		
//		if(stackTrace.length > 2) {
//			String calleeClass = stackTrace[1].getClassName();
//			String calleeMethod = stackTrace[1].getMethodName();
//			
//			for(int i = 2; i < stackTrace.length; i++) {
//				String methodName = stackTrace[i].getMethodName();
//				
//				if(methodName.equals("invoke") || methodName.equals("intercept")) {
//					continue;
//				}
//				
//				String className = stackTrace[i].getClassName();
//				
//				if(className.startsWith("org.springframework.")) {
//					continue;
//				}
//				
//				if(methodName.equals(calleeMethod) && className.startsWith(calleeClass)) {
//					continue;
//				}
//				
//				return className.equals(this.getClass().getName());
//			}
//		}
		
		return false;
	}
	
	@Override
	public boolean inCallerList() {
//		Throwable throwable = new Throwable();
//		StackTraceElement[] stackTrace = throwable.getStackTrace();
//		
//		if(stackTrace.length > 2) {
//			String calleeClass = stackTrace[1].getClassName();
//			String calleeMethod = stackTrace[1].getMethodName();
//			
//			for(int i = 2; i < stackTrace.length; i++) {
//				String methodName = stackTrace[i].getMethodName();
//				
//				if(methodName.equals("invoke") || methodName.equals("intercept")) {
//					continue;
//				}
//				
//				String className = stackTrace[i].getClassName();
//				
//				if(className.startsWith("org.springframework.")) {
//					continue;
//				}
//				
//				if(methodName.equals(calleeMethod) && className.startsWith(calleeClass)) {
//					continue;
//				}
//				
//				if(className.equals(this.getClass().getName())) {
//					return true;
//				}
//			}
//		}
		
		return false;
	}

	@Override
	public void cleanSynRelationRelatedEmptyFolders(String userId, SyncRelation syncRelation) {
		if(syncRelation.isCrossClientCloud()) {
			String folderId = "";
			if(!syncRelation.isSourceInClient()) {
				folderId = syncRelation.getMetadataIndexId();
			}
			if(!syncRelation.isDestinationInClient()) {
				folderId = syncRelation.getDestinationMetadataIndexId();
			}
			
			Metadata folder = metadataDao.getMetadata(folderId);
			List<Metadata> subFolders = metadataFactory.getAllSubFolders(folder);
			List<Metadata> emptyFolders = Lists.newArrayList(Iterables.filter(subFolders, new Predicate<Metadata>() {
				@Override
				public boolean apply(Metadata metadata) {
					return metadata.isFolder() && (metadata.getFilesCount() == 0);
				}
			}));
			
			for(Metadata emptyFolder : emptyFolders) {
				try {
					folderService.deleteFolder(emptyFolder.getId(), userId, false, false);
				}
				catch(MetadataException ex) {
					// ignore HttpStatus.FOLDER_NOT_EXIST
					if(ex.getHttpStatus() != HttpStatus.FOLDER_NOT_EXIST) {
						throw ex;
					}
				}
			}
		}
	}
	
	@Override
	public boolean hasSyncFolder(Metadata folder) {
		return hasSyncFolder(folder, false);
	}
	
	@Override
	public boolean hasSyncFolder(Metadata folder, boolean selfIncluded) {
		return hasSyncFolder(folder, selfIncluded, null);
	}
	
	@Override
	public boolean hasSyncFolder(Metadata folder, boolean selfIncluded, Collection<Metadata> subFolders) {
		if(folder == null) return false;
		if(!folder.isFolder()) return false;
		if(selfIncluded && !StringUtils.isNullOrEmpty(folder.getSyncRootId())) return true;
		if(subFolders == null) subFolders = metadataFactory.getAllSubFolders(folder);
		
		for(Metadata subFolder : subFolders) {
			if(!StringUtils.isNullOrEmpty(subFolder.getSyncRootId())) {
				return true;
			}
		}
		
		return false;
	}
	
	@Override
	public List<String> getAllSubSyncFolderId(BaseMetadata folder, boolean selfIncluded, Collection<? extends BaseMetadata> subFolders) {
		Set<String> resultSet = new HashSet<String>();
		
		if(folder == null) return new ArrayList<String>();
		if(!folder.isFolder()) return new ArrayList<String>();
		
		if(subFolders == null) {
			Metadata folderMetadata = new Metadata();
			folderMetadata.fromBaseMetadata(folder);
			subFolders = metadataFactory.getAllSubFolders(folderMetadata);
		}
		
		if(selfIncluded && folder.getId().equals(folder.getSyncRootId())) {
			if(!resultSet.contains(folder.getId())) {
				resultSet.add(folder.getId());
			}
		}
		
		for(BaseMetadata subFolder : subFolders) {
			if(subFolder.getId().equals(subFolder.getSyncRootId())) {
				if(!resultSet.contains(subFolder.getId())) {
					resultSet.add(subFolder.getId());
				}
			}
		}
		
		return new ArrayList<String>(resultSet);
	}
	
	// Private
	
	private void sendXmppMessage(PathRequestBase pathRequest, SystemEvent event, String syncRootId) {
		PathInfo pathInfo = pathFactory.parsePathInfo(pathRequest, true, true);
		if(!StringUtils.isNullOrEmpty(syncRootId)) {
			pathInfo.setSyncRootId(syncRootId);
		}
		sendXmppMessage(pathInfo, event);
	}
	
	private void sendXmppMessage(SyncRelationRequest syncRelationRequest, SystemEvent event, List<SyncRelation> syncRelations) {
		PathInfo pathInfo = pathFactory.parsePathInfo(syncRelationRequest, true, true);
		if (StringUtils.isNullOrEmpty(pathInfo.getFullOwnerPath())) {
			pathInfo = pathFactory.parsePathInfo(syncRelationRequest.parseDestinationToSyncRelationPathRequest(), true, true);
		}
		String syncRootId = "";
		for(SyncRelation syncRelation : syncRelations) {
			syncRootId = syncRelation.getDestinationOrSourceSyncRootId();
			if(!StringUtils.isNullOrEmpty(syncRootId)) {
				break;
			}
		}
		pathInfo.setSyncRootId(syncRootId);
		sendXmppMessage(pathInfo, event);
	}
	
	private void sendXmppMessage(BaseMetadata metadata, SystemEvent event, String userId) {
		if(metadata != null) {
			String fullOriginPath = metadata.getOwnerId() + "/" + metadataFactory.getMetadataOriginalPath(metadata);
			PathInfo pathInfo = new PathInfo(userId, metadata, fullOriginPath);
			sendXmppMessage(pathInfo, event);
		}
	}
	
	private void sendXmppMessage(PathInfo pathInfo, SystemEvent event) {
		xmppService.sendToAllDevices(event, pathInfo);
	}
	
	
	private void addActivityHistory(SyncRelation synRelation, String userId, ChangeReason reason) {
		if(!StringUtils.isNullOrEmpty(synRelation.getMetadataIndexId())) {
			addActivityHistory(synRelation.getMetadataIndexId(), userId, reason);
		}
		if(!StringUtils.isNullOrEmpty(synRelation.getDestinationMetadataIndexId())) {
			addActivityHistory(synRelation.getDestinationMetadataIndexId(), userId, reason);
		}
	}
	
	private void addActivityHistory(String metadataId, String userId, ChangeReason reason) {
		dynamoDBService.createActivityHistory(
				metadataDao.getFullPath(metadataId), "", "", "", metadataId,
				userId, DateUtils.nowUTCDateTime(), reason.toString(), "",
				ActivityHistoryLevel.NORMAL.toString());
	}
	
	private SyncRelation parseRequest(SyncRelationRequest syncRelationRequest) throws MetadataException {
		PathInfo sourcePathInfo = pathFactory.parsePathInfo(syncRelationRequest, true, true);
		PathInfo targetPathInfo = pathFactory.parsePathInfo(syncRelationRequest.parseDestinationToSyncRelationPathRequest(), true, true);
		
		SyncRelation syncRelation = new SyncRelation();
		
		syncRelation.setOwnerId(StringUtils.isNullOrEmpty(syncRelationRequest.getOwnerId()) ? syncRelationRequest.getUserId() : syncRelationRequest.getOwnerId());
		if(sourcePathInfo.isUnderMyDeviceSyncFolders()) {
			syncRelation.setSourcePath(syncRelationRequest.getPath());
		} else {
			try {
				BaseMetadata source = metadataFactory.getBaseMetadataByPath(sourcePathInfo);
				syncRelation.setMetadataIndexId(source == null ? null : source.getId());
			} catch(MetadataException me) {
				throw new MetadataException(HttpStatus.SOURCE_FOLDER_NOT_FOUND);
			}
		}
		
		syncRelation.setDestinationOwnerId(StringUtils.isNullOrEmpty(syncRelationRequest.getDestinationOwnerId()) ? syncRelationRequest.getUserId() : syncRelationRequest.getDestinationOwnerId());
		if(targetPathInfo.isUnderMyDeviceSyncFolders()) {
			syncRelation.setDestinationSourcePath(syncRelationRequest.getDestinationPath());
		} else {
			try {
				BaseMetadata destination = metadataFactory.getBaseMetadataByPath(targetPathInfo);
				syncRelation.setDestinationMetadataIndexId(destination == null ? null : destination.getId());
			} catch(MetadataException me) {
				throw new MetadataException(HttpStatus.TARGET_FOLDER_NOT_FOUND);
			}
		}
		
		syncRelation.setAction(SyncRelationAction.parse(syncRelationRequest.getAction()));
		syncRelation.setEnable(syncRelationRequest.isEnable());
		
		return syncRelation;
	}
	
	private SyncRelation addSyncRelation(SyncRelationRequest syncRelationRequest, boolean normalToSync)
			throws MetadataException {
		
		SyncRelation syncRelation = parseRequest(syncRelationRequest);
		Metadata source = checkSourceOfSyncRelation(syncRelation);
		Metadata target = checkTargetOfSyncRelation(syncRelation);
		
		// for transform normal to sync, the folders must NOT be with any relation
		if(normalToSync) {
			if(!syncRelation.isSourceInClient()) {
				if(source != null && !StringUtils.isNullOrEmpty(source.getSyncRootId())) {
					throw new MetadataException(HttpStatus.SOURCE_FOLDER_ALREADY_SYNC);
				}
			}
			if(!syncRelation.isDestinationInClient()) {
				if(target != null && !StringUtils.isNullOrEmpty(target.getSyncRootId())) {
					throw new MetadataException(HttpStatus.TARGET_FOLDER_ALREADY_SYNC);
				}
			}
		}
		
		// source and destination must be cross between cloud and client 
		if(!syncRelation.isCrossClientCloud()) {
			throw new MetadataException(HttpStatus.NOT_ALLOW_RELATION_IN_SAME_PLACE);
		}
		
		// only the auto sync relation is allowed
		if(syncRelation.getAction() != SyncRelationAction.SYNC) {
			throw new MetadataException(HttpStatus.RELATION_ACTION_NOT_ALLOW);
		}
		
		if(isAnyRelationExist(syncRelation)) {
			throw new MetadataException(HttpStatus.RELATION_EXIST);
		}
		if(willOccurCycle(syncRelation)) {
			throw new MetadataException(HttpStatus.CYCLE_SYNC);
		}
		
		if(source != null && target != null) {
			// TODO: do the first process after a relation created
		}
		
		// update sync_root_id and syncRootCount
		
		CountManageModel countManageModel = new CountManageModel();
		
		if(!syncRelation.isSourceInClient()) {
			metadataDao.updateSyncRootId(source.getId(), source.getId());
			for(Metadata metadata : metadataFactory.getAllSubFoldersAndFiles(source)) {
				metadataDao.updateSyncRootId(metadata.getId(), source.getId());
			}
			countManageModel.addFolder(source.getId(), 0, 0, 0, 0, 1, 0);
		}
		
		if(!syncRelation.isDestinationInClient()) {
			metadataDao.updateSyncRootId(target.getId(), target.getId());
			for(Metadata metadata : metadataFactory.getAllSubFoldersAndFiles(target)) {
				metadataDao.updateSyncRootId(metadata.getId(), target.getId());
			}
			countManageModel.addFolder(target.getId(), 0, 0, 0, 0, 1, 0);
		}
		
		countService.updateAllParentCount(countManageModel);
		
		// insert relation to db
		
		syncRelationDao.addSyncRelation(syncRelation);
		if(syncRelation.getAction() == SyncRelationAction.SYNC) {
			syncRelation.swapSourceTarget();
			syncRelationDao.addSyncRelation(syncRelation);
		}
		
		if(!normalToSync) {
			sendXmppMessage(syncRelationRequest, SystemEvent.ADD_SYNC_RELATION, Arrays.asList(syncRelation));
			addActivityHistory(syncRelation, syncRelationRequest.getUserId(), ChangeReason.ADD_SYNC_RELATION);
		}
		
		return syncRelation;
	}
	
	private Metadata checkSourceOfSyncRelation(SyncRelation syncRelation) throws MetadataException {
		if(StringUtils.isNullOrEmpty(syncRelation.getSourcePath()) && StringUtils.isNullOrEmpty(syncRelation.getMetadataIndexId())) {
			throw new MetadataException(HttpStatus.SOURCE_FOLDER_NOT_FOUND);
		}
		
		Metadata source = null;
				
		if(!syncRelation.isSourceInClient()) {
			source = metadataDao.getMetadata(syncRelation.getMetadataIndexId());
			
			if(!source.isFolder()) {
				throw new MetadataException(HttpStatus.SOURCE_IS_NOT_A_FOLDER);
			}
			
			if(!StringUtils.isNullOrEmpty(source.getSyncRootId())) {
				throw new MetadataException(HttpStatus.SOURCE_FOLDER_ALREADY_SYNC);
			}
			
			checkUpperLevelFolders(source);
			checkLowerLevelFolders(source);
		}
		else {
			if(syncRelationDao.getRelationsOnFolderOfClient(syncRelation.getOwnerId(), syncRelation.getSourcePath()).size() > 0) {
				throw new MetadataException(HttpStatus.SOURCE_FOLDER_ALREADY_SYNC);
			}
			if(syncRelationDao.getUpperLevelFoldersOfClient(syncRelation.getOwnerId(), syncRelation.getSourcePath()).size() > 0) {
				throw new MetadataException(HttpStatus.UPPER_LEVEL_OF_SOURCE_ALREADY_SYNC);
			}
			if(syncRelationDao.getLowerLevelFoldersOfClient(syncRelation.getOwnerId(), syncRelation.getSourcePath()).size() > 0) {
				throw new MetadataException(HttpStatus.LOWER_LEVEL_OF_SOURCE_ALREADY_SYNC);
			}
		}
		
		if(source != null && !StringUtils.isNullOrEmpty(source.getSharedRootId())) {
			throw new MetadataException(HttpStatus.CANT_AFFECT_ON_SHARED_FOLDER);
		}
		
		return source;
	}
	
	private Metadata checkTargetOfSyncRelation(SyncRelation syncRelation) throws MetadataException {
		if(StringUtils.isNullOrEmpty(syncRelation.getDestinationSourcePath()) && StringUtils.isNullOrEmpty(syncRelation.getDestinationMetadataIndexId())) {
			throw new MetadataException(HttpStatus.TARGET_FOLDER_NOT_FOUND);
		}
		
		Metadata target = null;
				
		if(!syncRelation.isDestinationInClient()) {
			target = metadataDao.getMetadata(syncRelation.getDestinationMetadataIndexId());
			
			if(!target.isFolder()) {
				throw new MetadataException(HttpStatus.TARGET_IS_NOT_A_FOLDER);
			}
			
			if(!StringUtils.isNullOrEmpty(target.getSyncRootId())) {
				throw new MetadataException(HttpStatus.TARGET_FOLDER_ALREADY_SYNC);
			}
			
			checkUpperLevelFolders(target);
			checkLowerLevelFolders(target);
		}
		else {
			if(syncRelationDao.getRelationsOnFolderOfClient(syncRelation.getDestinationOwnerId(), syncRelation.getDestinationSourcePath()).size() > 0) {
				throw new MetadataException(HttpStatus.SOURCE_FOLDER_ALREADY_SYNC);
			}
			if(syncRelationDao.getUpperLevelFoldersOfClient(syncRelation.getDestinationOwnerId(), syncRelation.getDestinationSourcePath()).size() > 0) {
				throw new MetadataException(HttpStatus.UPPER_LEVEL_OF_TARGET_ALREADY_SYNC);
			}
			if(syncRelationDao.getLowerLevelFoldersOfClient(syncRelation.getDestinationOwnerId(), syncRelation.getDestinationSourcePath()).size() > 0) {
				throw new MetadataException(HttpStatus.LOWER_LEVEL_OF_TARGET_ALREADY_SYNC);
			}
		}
		
		if(target != null && !StringUtils.isNullOrEmpty(target.getSharedRootId())) {
			throw new MetadataException(HttpStatus.CANT_AFFECT_ON_SHARED_FOLDER);
		}
		
		return target;
	}
	
	private boolean isAnyRelationExist(SyncRelation syncRelation) throws MetadataException {
		try {
			SyncRelation criteria = (SyncRelation) syncRelation.clone();
			criteria.setAction(null);
			criteria.setEnable(null);
			
			if(syncRelationDao.getSyncRelationByCriteria(criteria).size() > 0) {
				return true;
			}
			
			criteria.swapSourceTarget();
			
			if(syncRelationDao.getSyncRelationByCriteria(criteria).size() > 0) {
				return true;
			}
		} catch(Exception ex) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
		
		return false;
	}
	
	private boolean willOccurCycle(SyncRelation syncRelation) throws MetadataException {
		Set<String> visited = new HashSet<String>();
		Queue<SyncRelation> queue = new LinkedList<SyncRelation>();
		
		queue.add(syncRelation);
		visited.add(syncRelation.getSourceKey());
		if(syncRelation.getAction() == SyncRelationAction.SYNC) {
			visited.add(syncRelation.toRelationKey());
		}
		
		while((syncRelation = queue.poll()) != null) {
			String key = syncRelation.getDestinationKey();
			
			if(visited.contains(key)) {
				if(syncRelation.getAction() != SyncRelationAction.SYNC || !visited.contains(syncRelation.toRelationKey())) {
					return true;
				}
				continue;
			}
			
			visited.add(key);
			if(syncRelation.getAction() == SyncRelationAction.SYNC) {
				visited.add(syncRelation.toRelationKey());
			}
			
			SyncRelation criteria = new SyncRelation();
			criteria.setMetadataIndexId(syncRelation.getDestinationMetadataIndexId());
			criteria.setOwnerId(syncRelation.getDestinationOwnerId());
			criteria.setSourcePath(syncRelation.getDestinationSourcePath());
			queue.addAll(syncRelationDao.getSyncRelationByCriteria(criteria));
		}
		
		return false;
	}
	
	private void checkUpperLevelFolders(BaseMetadata folder) {
		String syncRootId = folder.getSyncRootId();
		if(!StringUtils.isNullOrEmpty(syncRootId) && !syncRootId.equals(folder.getId())) {
			throw new MetadataException(HttpStatus.UPPER_LEVEL_OF_TARGET_ALREADY_SYNC);
		}
		String sharedRootId = folder.getSharedRootId();
		if(!StringUtils.isNullOrEmpty(sharedRootId) && !sharedRootId.equals(folder.getId())) {
			throw new MetadataException(HttpStatus.CANT_AFFECT_ON_SHARED_FOLDER);
		}
	}
	
	private void checkLowerLevelFolders(Metadata folder) {
		List<Metadata> subFolders = metadataFactory.getAllSubFolders(folder);
		for(Metadata subfolder : subFolders) {
			if(!StringUtils.isNullOrEmpty(subfolder.getSyncRootId())) {
				throw new MetadataException(HttpStatus.LOWER_LEVEL_OF_TARGET_ALREADY_SYNC);
			}
			if(!StringUtils.isNullOrEmpty(subfolder.getSharedRootId())) {
				throw new MetadataException(HttpStatus.CANT_AFFECT_ON_SHARED_FOLDER);
			}
		}
	}
	
	private List<SyncRelation> getSyncRelationBySourceMetadataIndexId(String sourceMetadataIndexId) {
		SyncRelation criteria = new SyncRelation();
		criteria.setMetadataIndexId(sourceMetadataIndexId);
		return syncRelationDao.getSyncRelationByCriteria(criteria);
	}
	
	/**
	 * @param metadata
	 * @return splitted path, not include sync root
	 */
	private List<String> getRelativePathUnderSyncRoot(BaseMetadata metadata) {
		List<String> paths = null;
		
		if(!StringUtils.isNullOrEmpty(metadata.getSyncRootId())) {
			paths = new ArrayList<String>();
			while(true) {
				if(metadata.getSyncRootId().equals(metadata.getId())) {
					break;
				}
				paths.add(0, metadata.getOriginName());
				metadata = metadataDao.getMetadata(metadata.getParentId());
				if(metadata == null) {
					break;
				}
			}
		}
		
		return paths;
	}
	
	private void cleanSynRelationRelatedFiles(SyncRelation syncRelation) {
		if(syncRelation.isSourceInClient()) {
			for(Metadata file : metadataDao.getAllFilesInLinkFolder(syncRelation.getSourceFullSourcePath(), false)) {
				fileService.deleteFile(file);
			}
		}
		if(syncRelation.isDestinationInClient()) {
			for(Metadata file : metadataDao.getAllFilesInLinkFolder(syncRelation.getDestinationFullSourcePath(), false)) {
				fileService.deleteFile(file);
			}
		}
	}
	
	private void transformSyncToNormal(BaseMetadata folder) {
		if(folder == null) return;
		
		Metadata folderMetadata = new Metadata();
		folderMetadata.fromBaseMetadata(folder);
		
		metadataDao.transformSyncToNormal(folderMetadata.getId(), false);
		for(Metadata metadata : metadataFactory.getAllSubFoldersAndFiles(folderMetadata)) {
			metadataDao.transformSyncToNormal(metadata.getId(), false);
		}
	}
	
	private void addMetadataToCache(Map<String, Metadata> cache, Metadata metadata) {
		addMetadataToCache(cache, Arrays.asList(metadata));
	}
	
	private void addMetadataToCache(Map<String, Metadata> cache, List<Metadata> metadatas) {
		for(Metadata metadata : metadatas) {
			if(!cache.containsKey(metadata.getId())) {
				cache.put(metadata.getId(), metadata);
			}
		}
	}
	
	private void addSyncRelationsToMap(Map<String, SyncRelation> map, List<SyncRelation> syncRelations) {
		for(SyncRelation syncRelation : syncRelations) {
			if(!map.containsKey(syncRelation.toRelationKey())) {
				map.put(syncRelation.toRelationKey(), syncRelation);
			}
			else {
				// let local path be the source
				if(syncRelation.isSourceInClient()) {
					map.put(syncRelation.toRelationKey(), syncRelation);
				}
			}
		}
	}
	
	private void transformToNormalIfNoRelationExists(SyncRelation syncRelation) {
		transformToNormalIfNoRelationExists(syncRelation.getMetadataIndexId());
		transformToNormalIfNoRelationExists(syncRelation.getDestinationMetadataIndexId());
	}
	
	private void transformToNormalIfNoRelationExists(String metadataId) {
		if(!StringUtils.isNullOrEmpty(metadataId)) {
			List<SyncRelation> syncRelations = getSyncRelationsByMetadataId(metadataId);
			if(syncRelations.size() == 0) {
				transformSyncToNormal(metadataDao.getMetadata(metadataId));
			}
		}
	}
	
	private List<SyncRelation> getSyncRelationsByMetadataId(String MetadataId) {
		SyncRelation criteria = new SyncRelation();
		criteria.setMetadataIndexId(MetadataId);
		List<SyncRelation> result = new ArrayList<SyncRelation>();
		result.addAll(syncRelationDao.getSyncRelationByCriteria(criteria));
		result.addAll(syncRelationDao.getSyncRelationByCriteria(criteria.swapSourceTarget()));
		return result;
	}
	
	private List<SyncRelation> distinctSyncRelations(List<SyncRelation> syncRelations) {
		Map<String, SyncRelation> map = new HashMap<String, SyncRelation>();
		addSyncRelationsToMap(map, syncRelations);
		return new ArrayList<SyncRelation>(map.values());
	}
}
