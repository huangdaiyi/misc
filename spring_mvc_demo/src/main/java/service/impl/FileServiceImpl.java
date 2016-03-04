package service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import model.AlreadyRead;
import model.BackupMetadata;
import model.BaseMetadata;
import model.BulkFileInfoItem;
import model.CellPhone;
import model.CollaborateMember;
import model.CountManageModel;
import model.DefaultBackupSetting;
import model.ExistFile;
import model.FileProcessingStatus;
import model.Metadata;
import model.MetadataAttr;
import model.PathInfo;
import model.PathRequestBase;
import model.ReaderFileMetadata;
import model.ReaderPageDetail;
import model.RequestBase;
import model.TextViewSetting;
import model.request.AddExtensionBackupRequest;
import model.request.BulkActionRequest;
import model.request.BulkActionRequestItem;
import model.request.BulkCreateFileRequest;
import model.request.CopyFileRequest;
import model.request.DeleteExtensionBackupRequest;
import model.request.DeleteFolderRequest;
import model.request.EncryptRequest;
import model.request.ExistFileRequest;
import model.request.GetAllDeviceRequest;
import model.request.GetBulkFileInfoRequest;
import model.request.GetFileProcessingStatusRequest;
import model.request.GetFileRequest;
import model.request.GetTextViewSettingRequest;
import model.request.MoveFileRequest;
import model.request.RenameFileRequest;
import model.request.RestoreBackupRequest;
import model.request.UpdateBackupCountRequest;
import model.request.UpdateFileProcessingStatusRequest;
import model.request.UpdateFileRequest;
import model.request.UpdateImageRequest;
import model.request.UpdateNoteRequest;
import model.request.UpdateReaderFileMetadataRequest;
import model.request.UpdateTextViewSettingRequest;
import model.response.AlreadyReadResponse;
import model.response.BackupCountResponse;
import model.response.BulkCreateFileResponse;
import model.response.FileResponse;
import model.response.GetBulkFileInfoResponse;
import model.response.GetDeviceResponse;
import model.response.GetExtensionBackupResponse;
import model.response.GetFileProcessingStatusResponse;
import model.response.GetReaderFileMetadataResponse;
import model.response.NoteResponse;

import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import service.BackupMetadataService;
import service.CollaborateService;
import service.CountService;
import service.DeviceService;
import service.DynamoDBService;
import service.EmailService;
import service.FileProcessingStatusService;
import service.FileService;
import service.FolderService;
import service.GatewayService;
import service.MQService;
import service.MetadataAttrService;
import service.SSOService;
import service.SyncRelationService;
import service.XmppService;
import utils.DateUtils;
import utils.StringUtils;
import constants.ActivityHistoryLevel;
import constants.ChangeReason;
import constants.CommonFolders;
import constants.HttpStatus;
import constants.MetadataType;
import constants.ProcessAction;
import constants.ProcessingStatus;
import constants.SystemEvent;
import dao.AlreadyReadDao;
import dao.BackupMetadataDao;
import dao.CollaborateDao;
import dao.DeviceDao;
import dao.ExtensionBackupDao;
import dao.FileProcessingStatusDao;
import dao.MetadataAttrDao;
import dao.MetadataDao;
import dao.ReaderFileMetadataDao;
import dao.TextViewSettingDao;
import exception.MetadataException;
import factory.MetadataFactory;
import factory.PathFactory;

@Service
public class FileServiceImpl implements FileService {

	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private ReaderFileMetadataDao readerFileMetadataDao;
	@Autowired
	private BackupMetadataService backupMetadataService;
	@Autowired
	private MetadataAttrService metadataAttrService;
	@Autowired
	private MetadataAttrDao metadataAttrDao;
	@Autowired
	private PathFactory pathFactory;
	@Autowired
	private MetadataFactory metadataFactory;
	@Autowired
	private MQService mqService;
	@Autowired
	private BackupMetadataDao backupMetadataDao;
	@Autowired
	private FolderService folderService;
	@Autowired
	private DeviceDao deviceDao;
	@Autowired
	private DeviceService deviceService;
	@Autowired
	private FileProcessingStatusService fileProcessingStatusService;
	@Autowired
	private FileProcessingStatusDao fileProcessingStatusDao;
	@Autowired
	private GatewayService gatewayService;
	@Autowired
	private CountService countService;
	@Autowired
	private SSOService ssoService;
	@Autowired
	private XmppService xmppService;
	@Autowired
	private DynamoDBService dynamoDBService;
	@Autowired
	private CollaborateService collaborateService;
	@Autowired
	private SyncRelationService syncRelationService;
	@Autowired
	private TextViewSettingDao textViewSettingDao;
	@Autowired
	private AlreadyReadDao alreadyReadDao;
	@Autowired
	private CollaborateDao collaborateDao;
	@Autowired
	private EmailService emailService;
	@Autowired
	private ExtensionBackupDao extensionBackupDao;

	@Override
	public FileResponse getFile(GetFileRequest getFileRequest) throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(getFileRequest, false, true);
		BaseMetadata metadata = metadataFactory.getFileByPathInfo(pathInfo);
		String parentPath = FilenameUtils.getFullPathNoEndSeparator(metadataFactory.getMetadataRelativePath(metadata, getFileRequest.getUserId()));
		FileResponse fileResponse = metadataFactory.toFileResponses(getFileRequest.getToken(), Arrays.asList(metadata), true, parentPath).get(0);
		metadataFactory.adjustFileResponsesItemStyleByGlobalItemStyle(Arrays.asList(fileResponse), pathInfo.getOwnerId());
		return updateAlreadyReadAndCount(getFileRequest.getUserId(), metadata, fileResponse);
	}

	@Override
	@Transactional
	public FileResponse createFile(UpdateFileRequest updateFileRequest)
			throws MetadataException {
		pathFactory.checkCanCreateFile(updateFileRequest.getPath());
		Metadata parentMetadata = null;
		PathInfo pathInfo = pathFactory.parsePathInfo(updateFileRequest, false, false);
		// check file exist
		if (pathInfo.isUnderMyAppSyncFolders()){
			if (metadataDao.getSyncAppMetadataByPath(pathInfo.getFullOwnerPath(),
					pathInfo.getDeviceUid(), pathInfo.getFullSourcePath()) != null) {
				throw new MetadataException(HttpStatus.FILE_EXIST);
			}
		}else{
			if (metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
					pathInfo.getDeviceUid(), pathInfo.getFullSourcePath()) != null) {
				throw new MetadataException(HttpStatus.FILE_EXIST);
			}
		}
		// get parent ID
		String parentPath = FilenameUtils.getFullPathNoEndSeparator(pathInfo.getFullOwnerPath());
		if (pathInfo.isUnderMyAppSyncFolders()){
			parentMetadata = metadataDao.getSyncAppMetadataByPath(parentPath, pathInfo.getDeviceUid(),FilenameUtils.getPathNoEndSeparator(pathInfo.getFullSourcePath()));
		}else{
		    parentMetadata = metadataDao.getMetadataByPath(parentPath, "", "");
		}
		
		if (!parentMetadata.isFolder()) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}
		
		//get max sort priority
		int sortPriority = metadataDao.getMaxSortPriority(parentMetadata.getId());
		Metadata fileMetadata = metadataFactory.buildBase4Create(pathInfo);
		if (pathInfo.isUnderMyDeviceSyncFolders()) {
			if (!pathInfo.isUnderMyAppSyncFolders()) {
				// create mobile folders if not exist
				if (deviceDao.getMobileFolders(updateFileRequest.getUserId(),
						FilenameUtils.getFullPathNoEndSeparator(pathInfo
								.getFullSourcePath())) == null) {
					deviceDao.createMobileFolders(updateFileRequest.getUserId(),
							FilenameUtils.getFullPathNoEndSeparator(pathInfo
									.getFullSourcePath()), pathInfo.getDeviceUid());
				}
			}			
		}
				
		String params = StringUtils.writeJSON(updateFileRequest.getParams());
		fileMetadata.setParentId(parentMetadata.getId());
		fileMetadata.setSharedRootId(parentMetadata.getSharedRootId());
		fileMetadata.setSize(updateFileRequest.getSize());
		fileMetadata.setTotalSize(updateFileRequest.getSize());
		fileMetadata.setIconBlockId(updateFileRequest.getIconBlockId());
		fileMetadata.setIconText(updateFileRequest.getIconText());
		fileMetadata.setIconTextColor(updateFileRequest.getIconTextColor());
		fileMetadata.setIconTextStyle(updateFileRequest.getIconTextStyle());
		fileMetadata.setSortPriority(sortPriority+1);
		fileMetadata.setNote(updateFileRequest.getNote());
		fileMetadata.setSyncRootId(parentMetadata.getSyncRootId());
		fileMetadata.setParams(StringUtils.isNullOrEmpty(params) ? "" : params);
		fileMetadata.setEncrypted(updateFileRequest.isEncrypted());
		fileMetadata.setBlockId(updateFileRequest.getBlockId());
		if(pathInfo.isUnderMyDeviceSyncFolders()) {
			fileMetadata.setType(MetadataType.LINKFILE.toString());
		}
		
		if(!StringUtils.isNullOrEmpty(parentMetadata.getSyncRootId()) && fileMetadata.getType().equals(MetadataType.NORMAL.toString())) {
			// not allow to create a normal file in a internal sync folder
			throw new MetadataException(HttpStatus.CANT_CREATE_FILE_IN_THIS_FOLDER);
		}
		metadataDao.createMetadata(fileMetadata);
		
		// create attr
        metadataAttrDao.createMetadataAttr(new MetadataAttr(fileMetadata.getId(), false, 0, 1, fileMetadata.getSize(),0,0,0,0,0));
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.addFile(fileMetadata.getParentId(), fileMetadata.getSize());
		countService.updateAllParentCount(countManageModel); 
		// backup file
		if (!updateFileRequest.isEncrypted()){
			//backupMetadataService.backupMetadata(fileMetadata,updateFileRequest.getUserId(),true);
			backupMetadataService.createSingleBackup(fileMetadata, updateFileRequest.getUserId());
			// extension
			addExtensionBackupByCreateFile(pathInfo.getOwnerId(),pathInfo.getFullOriginPath());
		}
		
		//  create active history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(parentMetadata.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", fileMetadata.getId(),
				pathInfo.getUserId(),
				String.valueOf(DateUtils.nowUTCDateTime()),
				ChangeReason.CREATE_FILE.toString(),
				fileMetadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());

		// send MQ
		if (!fileMetadata.isEncrypted()) {
			mqService.sendForStreaming(fileMetadata, updateFileRequest.getToken());
			mqService.sendForReader(fileMetadata, updateFileRequest.getToken());
			mqService.sendAddFullTextSearch(fileMetadata);
			mqService.sendForThumbnail(fileMetadata);
		}
		
		//send xmpp
		xmppService.sendToAllDevices(SystemEvent.UPDATE_FILE, pathInfo);
		
		// sync
		if(!syncRelationService.isCaller()) {
			syncRelationService.sync(fileMetadata, SystemEvent.CREATE_FILE);
		}
		parentPath = FilenameUtils.getFullPathNoEndSeparator(pathInfo.getFullOriginPath());
		return metadataFactory.toFileResponse(fileMetadata, true, StringUtils.trimFilePathRoot( parentPath));
	}

	@Override
	@Transactional
	public FileResponse updateFile(UpdateFileRequest updateFileRequest)
			throws MetadataException {
		
		PathInfo pathInfo = pathFactory.parsePathInfo(updateFileRequest, false, true);
		// get and check file
		Metadata originFileMetadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (originFileMetadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (originFileMetadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		
		String updateBlockId =StringUtils.isNullOrEmpty(updateFileRequest.getBlockId())?originFileMetadata.getBlockId():updateFileRequest.getBlockId() ;
		String updateNote =StringUtils.isNullOrEmpty(updateFileRequest.getNote())?originFileMetadata.getNote():updateFileRequest.getNote() ;
		Long updateSize = updateFileRequest.getSize() == 0L?originFileMetadata.getSize():updateFileRequest.getSize();
		String iconBlockId = StringUtils.isNullOrEmpty(updateFileRequest.getIconBlockId())?originFileMetadata.getIconBlockId():updateFileRequest.getIconBlockId();
		String iconText = StringUtils.isNullOrEmpty(updateFileRequest.getIconText())?originFileMetadata.getIconText():updateFileRequest.getIconText();
		String iconTextColor = StringUtils.isNullOrEmpty(updateFileRequest.getIconTextColor())?originFileMetadata.getIconTextColor():updateFileRequest.getIconTextColor();
		String iconTextStyle = StringUtils.isNullOrEmpty(updateFileRequest.getIconTextStyle())?originFileMetadata.getIconTextStyle():updateFileRequest.getIconTextStyle();
		String params = StringUtils.writeJSON(updateFileRequest.getParams());
		String originalBlockId = originFileMetadata.getBlockId();
		
		Long deltaSize = updateSize - originFileMetadata.getSize();
		
		originFileMetadata.setModifiedAt(DateUtils.nowUTCTimestamp());
		originFileMetadata.setModifiedBy(pathInfo.getUserId());
		originFileMetadata.setSize(updateSize);
		originFileMetadata.setBlockId(updateBlockId);
		originFileMetadata.setTotalSize(updateSize);
		originFileMetadata.setIconBlockId(iconBlockId);
		originFileMetadata.setIconText(iconText);
		originFileMetadata.setIconTextColor(iconTextColor);
		originFileMetadata.setIconTextStyle(iconTextStyle);
		originFileMetadata.setParams(StringUtils.isNullOrEmpty(params) ?originFileMetadata.getParams() : params);
		originFileMetadata.setEncrypted(updateFileRequest.isEncrypted());
		originFileMetadata.setNote(updateNote);
		
		metadataDao.updateFileMetadata(originFileMetadata);
		if (deltaSize != 0) {
			// update attr
			
			metadataAttrDao.updateMetadataAttrOnOrigin(new MetadataAttr(originFileMetadata.getId(), false, 0, 0, deltaSize, 0,0,0,0,0));
			// update parents' size
			CountManageModel countManageModel = new CountManageModel();
			countManageModel.updateFile(originFileMetadata.getParentId(), deltaSize);
			countService.updateAllParentCount(countManageModel); 
		}
				
		// if update file to encrypted,delete all backup file
		if (updateFileRequest.isEncrypted()) {
			backupMetadataService.deleteFileOldBackups(originFileMetadata);
		}else {
			backupMetadataService.backupMetadata(originFileMetadata, pathInfo.getUserId());			
		}
		
		String parentPath = StringUtils.trimFilePathRoot(FilenameUtils.getFullPathNoEndSeparator(pathInfo
				.getFullOwnerPath()));
		//  create active history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(originFileMetadata.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", originFileMetadata.getId(),
				pathInfo.getUserId(), DateUtils.nowUTCDateTime(),
				ChangeReason.UPDATE_FILE.toString(),
				originFileMetadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
		
		// send MQ
		if (!originFileMetadata.getBlockId().equals(originalBlockId)) {
			mqService.sendForStreaming(originFileMetadata, updateFileRequest.getToken());
			mqService.sendForReader(originFileMetadata, updateFileRequest.getToken());
			mqService.sendAddFullTextSearch(originFileMetadata);
			mqService.sendForThumbnail(originFileMetadata);
		}
		
		//send xmpp
		xmppService.sendToAllDevices(SystemEvent.UPDATE_FILE, pathInfo);
		// sync
		if(!syncRelationService.isCaller()) {
			syncRelationService.sync(updateFileRequest, originFileMetadata, SystemEvent.UPDATE_FILE);
		}
		return metadataFactory.toFileResponse(originFileMetadata, true, parentPath);
	}

	@Override
	public FileResponse deleteFile(PathRequestBase deleteFileRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(deleteFileRequest, false, true);
		BaseMetadata baseMetadata = metadataFactory.getFileByPathInfo(pathInfo);
		deleteFile(baseMetadata);
				
		// send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(baseMetadata.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", baseMetadata.getId(),
				pathInfo.getUserId(), DateUtils.nowUTCDateTime(),
				ChangeReason.DELETE_FILE.toString(),
				baseMetadata.getSharedRootId(), ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.DELETE_FILE, pathInfo);
		
		String parentPath = StringUtils.trimFilePathRoot(FilenameUtils.getFullPathNoEndSeparator(pathInfo.getFullOwnerPath()));
		return metadataFactory.toFileResponse(baseMetadata, true, parentPath);
	}
	
	public void deleteFile(BaseMetadata baseMetadata) {
		// delete file
		if (baseMetadata instanceof Metadata) {
			deleteSingleFile((Metadata)baseMetadata);
		} else {
			deleteSingleBackupFile((BackupMetadata)baseMetadata);
		}
	}

	@Transactional
	@Override
	public void deleteSingleFile(Metadata metadata) {
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao.getFileProcessingStatus(metadata.getId());
		if (fileProcessingStatus != null && fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}
		metadataDao.deleteMetadata(metadata.getId());
		metadataAttrService.deleteMetadataAttr(metadata.getId());
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.deleteFile(metadata.getParentId(), metadata.getSize());
		countService.updateAllParentCount(countManageModel); 
		// unbackup file
		backupMetadataService.unbackupMetadata(metadata.getId());
		
		// sync
		if(!syncRelationService.inCallerList()) {
			syncRelationService.sync(metadata, SystemEvent.DELETE_FILE);
		}
	}
	
	@Transactional
	private void deleteSingleBackupFile(BackupMetadata backupMetadata) {
		backupMetadataDao.deleteBackupById(backupMetadata.getId());
		metadataAttrService.deleteMetadataAttr(backupMetadata.getId());
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.deleteFile(backupMetadata.getParentId(), backupMetadata.getSize());
		countService.updateAllParentCountBackup(countManageModel); 
	}

	@Override
	@Transactional
	public FileResponse renameFile(RenameFileRequest renameFileRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(renameFileRequest, false, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		// check : if under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao.getFileProcessingStatus(metadata.getId());
		if (fileProcessingStatus != null && fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}
		
		String ownerFolderPath = FilenameUtils.getFullPath(pathInfo
				.getFullOwnerPath());
		String newFullPath = new StringBuilder().append(ownerFolderPath)
				.append(renameFileRequest.getNewName()).toString();
		String syncFolderPath = metadata.getFullSourcePath();
		String newFullSourcePath = "";
		if (!StringUtils.isNullOrEmpty(syncFolderPath)) {
			newFullSourcePath = new StringBuilder()
			.append(FilenameUtils.getFullPath(syncFolderPath))
			.append(renameFileRequest.getNewName().toLowerCase()).toString();
		}
		Metadata fileNameSearchResult = metadataDao.getMetadataByPath(
				newFullPath, pathInfo.getDeviceUid(),
				newFullSourcePath);
		if (fileNameSearchResult != null) {
			throw new MetadataException(HttpStatus.FILE_EXIST);
		} else if (StringUtils.isNullOrEmpty(metadata.getName())) {
			throw new MetadataException(HttpStatus.NAME_IS_EMPTY);
		} else {
			if (!"".equals(metadata.getFullSourcePath())){
				metadata.setFullSourcePath(newFullSourcePath);
			}
			metadataDao.updateNewName(metadata.getId(),
					renameFileRequest.getNewName(), metadata.getFullSourcePath());
			metadata.setName(renameFileRequest.getNewName().toLowerCase());
			metadata.setOriginName(renameFileRequest.getNewName());
			backupMetadataService.renameBackup(metadata);
		}
		String parentPath = FilenameUtils.getFullPathNoEndSeparator(pathInfo
				.getFullOwnerPath());

		// send dynamoDB history for target file
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata.getSharedRootId());
		dynamoDBService.createActivityHistory(newFullPath,
				pathInfo.getFullOwnerPath(), sharedUpperPath, sharedUpperPath,
				metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.RENAME_FILE.toString(),
				metadata.getSharedRootId(), ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		PathInfo newPathInfo = pathInfo.clone();
		newPathInfo.setFullOwnerPath(newFullPath);
		newPathInfo.setFullSourcePath(newFullSourcePath);
		xmppService.sendToAllDevices(SystemEvent.RENAME_FILE, pathInfo, newPathInfo);
		// sync
		if(!syncRelationService.isCaller()) {
			syncRelationService.sync(renameFileRequest, metadata, SystemEvent.RENAME_FILE);
		}
		return metadataFactory.toFileResponse(metadata, true, parentPath);
	}

	@Override
	@Transactional
	public FileResponse moveFile(MoveFileRequest moveFileRequest)
			throws MetadataException {
		String userId = moveFileRequest.getUserId();
		String path = moveFileRequest.getPath();
		String destination = moveFileRequest.getDestination();
		String targetSharedRootId = moveFileRequest.getDestinationSharedRootId();
		String targetOwnerId = StringUtils.isNullOrEmpty(moveFileRequest
				.getDestinationOwnerId()) ? userId : moveFileRequest
				.getDestinationOwnerId();
		pathFactory.checkCanMoveFileFrom(path);
		pathFactory.checkCanMoveCopyFileTo(destination);
		PathInfo pathInfo = pathFactory.parsePathInfo(moveFileRequest, false, true);
		Metadata sourceFile = (Metadata)metadataFactory.getFileByPathInfo(pathInfo);
		// check source file: if under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao.getFileProcessingStatus(sourceFile.getId());
		if (fileProcessingStatus != null && fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}
		
		Metadata targetFolder = (Metadata)metadataFactory.getFolder(userId, destination,
				targetOwnerId, targetSharedRootId);
		
		if (!StringUtils.isNullOrEmpty(targetFolder.getSyncRootId())) {
			if(StringUtils.isNullOrEmpty(sourceFile.getSyncRootId())) {
				throw new MetadataException(HttpStatus.CANT_MOVE_TO_SYNC_FOLDER);
			}
			else if(!sourceFile.getSyncRootId().toLowerCase().equals(targetFolder.getSyncRootId().toLowerCase())) {
				throw new MetadataException(HttpStatus.CANT_MOVE_TO_OTHER_SYNC_FOLDER);
			}
		}
				
		String targetFilePath = StringUtils.concatFilePath(destination,
				FilenameUtils.getName(path));
		Metadata targetFile = (Metadata)metadataFactory.getBaseMetadata(userId, targetFilePath, "", targetOwnerId, targetSharedRootId, false);
		// check: target is folder & force_override = true
		if (targetFile != null && targetFile.isFolder() == true) {
			if (moveFileRequest.isOverride() && moveFileRequest.isForceOverride()){
				DeleteFolderRequest deleteFolderRequest = new DeleteFolderRequest();
				deleteFolderRequest.setToken(moveFileRequest.getToken());
				deleteFolderRequest.setPath(metadataFactory.getMetadataOriginalPath(targetFile));
				deleteFolderRequest.setOwnerId(targetFile.getOwnerId());
				deleteFolderRequest.setSharedRootId(targetSharedRootId);
				deleteFolderRequest.setExtract(false);
				deleteFolderRequest.setUserId(moveFileRequest.getUserId());
				folderService.deleteFolder(deleteFolderRequest);
				
				targetFile = null;
			}else{
			    throw new MetadataException(HttpStatus.TARGET_PATH_IS_FOLDER);
			}
		}
		// check: target file exists, override=true, target file is encrypted
		if (targetFile != null && moveFileRequest.isOverride() && targetFile.isEncrypted()) {
			throw new MetadataException(HttpStatus.VIOLATE_ENCRYPTION_RULE);
		}
				
		// move file
		String sharedRootId = targetFolder.getSharedRootId();
		String syncRootId = targetFolder.getSyncRootId();
		if (targetFile == null) {
			// 1. create target file
			targetFile = createTargetFileBySourceFile(sourceFile,
					targetFolder.getId(), null, userId, targetOwnerId,
					sharedRootId, syncRootId, false);
		} else {
			if (moveFileRequest.isOverride() == false) {
				// 1. generate new name, create target file
				String newName = metadataFactory.generateNameForMoveCopy(
						targetFile, false, true);
				targetFile = createTargetFileBySourceFile(sourceFile,
						targetFolder.getId(), newName, userId, targetOwnerId,
						sharedRootId, syncRootId, false);

			} else {
				// 1. update target file
				targetFile = updateTargetFileBySourceFile(sourceFile, targetFile, userId,
						targetOwnerId, sharedRootId, syncRootId, false);
			}
		}
		// 2. delete source file
		deleteSingleFile(sourceFile);
		
		if (targetFile.isEncrypted()) {
			backupMetadataService.deleteFileOldBackups(targetFile);
		}
		
		// send dynamoDB history
		String targetFullOwnerPath = StringUtils.concatFilePath(targetFile.getOwnerId(), metadataFactory.getMetadataOriginalPath(targetFile));
		String oldSharedUpperPath = metadataFactory.getSharedUpperPath(sourceFile.getSharedRootId());
		String sharedUpperPath = metadataFactory.getSharedUpperPath(targetFile.getSharedRootId());
		dynamoDBService.createActivityHistory( targetFullOwnerPath,pathInfo.getFullOwnerPath(),sharedUpperPath,oldSharedUpperPath,
				targetFile.getId(), pathInfo.getUserId(), DateUtils.nowUTCDateTime(), ChangeReason.MOVE_FILE.toString(), 
				targetFile.getSharedRootId(), ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		PathInfo newPathInfo = new PathInfo();
		newPathInfo.setFullOwnerPath(targetFullOwnerPath);
		newPathInfo.setOwnerId(targetOwnerId);
		newPathInfo.setSharedRootId(targetSharedRootId);
		xmppService.sendToAllDevices(SystemEvent.MOVE_FILE, pathInfo, newPathInfo);
		
		return metadataFactory.toFileResponse(targetFile, true, destination);
	}

	@Override
	@Transactional
	public FileResponse copyFile(CopyFileRequest copyFileRequest)
			throws MetadataException {
		String userId = copyFileRequest.getUserId();
		String path = copyFileRequest.getPath();
		String destination = copyFileRequest.getDestination();
		String targetSharedRootId = copyFileRequest.getDestinationSharedRootId();
		String targetOwnerId = StringUtils.isNullOrEmpty(copyFileRequest
				.getDestinationOwnerId()) ? userId : copyFileRequest
				.getDestinationOwnerId();
		pathFactory.checkCanMoveCopyFileTo(destination);		
		PathInfo pathInfo = pathFactory.parsePathInfo(copyFileRequest, false, true);
		BaseMetadata sourceFile = metadataFactory.getFileByPathInfo(pathInfo);
		if (sourceFile instanceof BackupMetadata) {
			String originName = StringUtils.formatName(sourceFile.getOriginName(), ((BackupMetadata) sourceFile).getBackupNo(), false);
			sourceFile.setOriginName(originName);
			sourceFile.setName(originName.toLowerCase());
		}
		// check source file: if under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao.getFileProcessingStatus(sourceFile.getId());
		if (fileProcessingStatus != null && fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}
		
		Metadata targetFolder = (Metadata)metadataFactory.getFolder(userId, destination,
				targetOwnerId, targetSharedRootId);
		
		if (!StringUtils.isNullOrEmpty(targetFolder.getSyncRootId())) {
			if(StringUtils.isNullOrEmpty(sourceFile.getSyncRootId())) {
				throw new MetadataException(HttpStatus.CANT_COPY_TO_SYNC_FOLDER);
			}
			else if(!targetFolder.getSyncRootId().toLowerCase().equals(sourceFile.getSyncRootId().toLowerCase())) {
				throw new MetadataException(HttpStatus.CANT_COPY_TO_OTHER_SYNC_FOLDER);
			}
		}
				
		String targetFilePath = StringUtils.concatFilePath(destination,
				FilenameUtils.getName(path));
		Metadata targetFile = (Metadata)metadataFactory.getBaseMetadata(userId, targetFilePath, "", targetOwnerId, targetSharedRootId, false);
		// check: target is folder & force_override = true
		if (targetFile != null && targetFile.isFolder() == true) {
			if (copyFileRequest.isOverride() && copyFileRequest.isForceOverride()){
				DeleteFolderRequest deleteFolderRequest = new DeleteFolderRequest();
				deleteFolderRequest.setToken(copyFileRequest.getToken());
				deleteFolderRequest.setPath(metadataFactory.getMetadataOriginalPath(targetFile));
				deleteFolderRequest.setOwnerId(targetFile.getOwnerId());
				deleteFolderRequest.setSharedRootId(targetSharedRootId);
				deleteFolderRequest.setExtract(false);
				deleteFolderRequest.setUserId(copyFileRequest.getUserId());
				folderService.deleteFolder(deleteFolderRequest);
				
				targetFile = null;
			}else{
			    throw new MetadataException(HttpStatus.TARGET_PATH_IS_FOLDER);
			}
		}
		// check: target file exists, override=true, target file is encrypted
		if (targetFile != null && copyFileRequest.isOverride() && targetFile.isEncrypted()) {
			throw new MetadataException(HttpStatus.VIOLATE_ENCRYPTION_RULE);
		}
		
		// copy file
		boolean fromBackup = pathInfo.isUnderMyBackupData();
		targetFile = copySingleFile(sourceFile, targetFile, targetFolder,
				userId, targetOwnerId, copyFileRequest.isOverride(), fromBackup);
		
		if (targetFile.isEncrypted()) {
			backupMetadataService.deleteFileOldBackups(targetFile);
		}
		
		//  send dynamoDB history
		String targetFullOwnerPath = StringUtils.concatFilePath(targetFile.getOwnerId(), metadataFactory.getMetadataOriginalPath(targetFile));
		String sharedUpperPath = metadataFactory.getSharedUpperPath(targetFile.getSharedRootId());
		String OldSharedUpperPath = metadataFactory.getSharedUpperPath(sourceFile.getSharedRootId());
		dynamoDBService.createActivityHistory(targetFullOwnerPath,
				pathInfo.getFullOwnerPath(), sharedUpperPath, OldSharedUpperPath,
				targetFile.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(), ChangeReason.COPY_FILE.toString(),
				targetFile.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		PathInfo newPathInfo = new PathInfo();
		newPathInfo.setFullOwnerPath(targetFullOwnerPath);
		newPathInfo.setOwnerId(targetOwnerId);
		newPathInfo.setSharedRootId(targetSharedRootId);
		xmppService.sendToAllDevices(SystemEvent.COPY_FILE, pathInfo, newPathInfo);
		
		return metadataFactory.toFileResponse(targetFile, true, destination);
	}

	@Override
	public Metadata copySingleFile(BaseMetadata sourceFile, Metadata targetFile, Metadata targetFolder,
			String userId, String targetOwnerId, boolean isOverride, boolean fromBackup){

		String sharedRootId = targetFolder.getSharedRootId();
		String syncRootId = targetFolder.getSyncRootId();
		if (targetFile != null) {
			if (targetFile.isFolder() == true) {
				throw new MetadataException(HttpStatus.TARGET_PATH_IS_FOLDER);
			}
			if (isOverride) { // update file
				updateTargetFileBySourceFile(sourceFile, targetFile, userId,
						targetOwnerId, sharedRootId, syncRootId, fromBackup);
			} else { // generate new name, create file
				String newName = metadataFactory.generateNameForMoveCopy(
						targetFile, false, false);
				targetFile = createTargetFileBySourceFile(sourceFile,
						targetFolder.getId(), newName, userId, targetOwnerId,
						sharedRootId, syncRootId, fromBackup);
			}
		} else { // create file
			targetFile = createTargetFileBySourceFile(sourceFile,
					targetFolder.getId(), null, userId, targetOwnerId,
					sharedRootId, syncRootId, fromBackup);
		}
		
		return targetFile;
	}
	
	@Override
	public Metadata updateTargetFileBySourceFile(BaseMetadata sourceFile,
			Metadata targetFile, String userId, String targetOwnerId,
			String sharedRootId, String syncRootId, boolean fromBackup) {
		long deltaSize = sourceFile.getSize() - targetFile.getSize();
		// 1. update target file & attr
		targetFile.setSize(sourceFile.getSize());
		targetFile.setOriginName(sourceFile.getOriginName());
		targetFile.setModifiedAt(DateUtils.nowUTCTimestamp());
		targetFile.setModifiedBy(userId);
		targetFile.setNote(sourceFile.getNote());
		targetFile.setParams(sourceFile.getParams());
		targetFile.setIconBlockId(sourceFile.getIconBlockId());
		targetFile.setIconText(sourceFile.getIconText());
		targetFile.setIconTextColor(sourceFile.getIconTextColor());
		targetFile.setIconTextStyle(sourceFile.getIconTextStyle());
		targetFile.setEncrypted(sourceFile.isEncrypted());
		targetFile.setBlockId(sourceFile.getBlockId());
		targetFile.setDeviceUid("");
		targetFile.setFullSourcePath("");
		targetFile.setType(MetadataType.NORMAL.toString());
		targetFile.setSharedRootId(sharedRootId);
		targetFile.setSyncRootId(syncRootId);
		if (sourceFile instanceof Metadata) {
			targetFile.setBackupCount(((Metadata)sourceFile).getBackupCount());
		}
		metadataDao.updateFileMetadata(targetFile);
		// update attr
		
		MetadataAttr attr = metadataAttrDao.getMetadataAttr(sourceFile.getId());
		attr.setMetadataIndexId(targetFile.getId());
		metadataAttrDao.updateMetadataAttr(attr);
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.updateFile(targetFile.getParentId(), deltaSize);
		countService.updateAllParentCount(countManageModel); 
		// 2. update target backup & backup attr, update parent of target backup
		//backupMetadataService.backupMetadata(targetFile, targetOwnerId,true);
		backupMetadataService.createSingleBackup(targetFile, targetOwnerId);
		// 3. copy association of file & backup
		String sourceFileId = sourceFile.getId();
		String targetFileId = targetFile.getId();
		String sourceBackupFileId = null;
		if (fromBackup){
			sourceBackupFileId = sourceFileId;
		}else{
			BackupMetadata sourceBackupMetadata = backupMetadataDao.getLatestBackup(sourceFileId);
			if (sourceBackupMetadata != null){
				sourceBackupFileId = sourceBackupMetadata.getId();
			}
		}
		BackupMetadata targetBackupFile = backupMetadataDao.getLatestBackup(
				targetFileId);
		readerFileMetadataDao.copyFileProcessingStatus(sourceFileId,
				targetFileId);
		readerFileMetadataDao
				.copyReaderFileMetadata(sourceFileId, targetFileId);
		readerFileMetadataDao.copyReaderFileMetadataDetail(sourceFileId,
				targetFileId);
		textViewSettingDao.copyTextViewSetting(sourceFileId, targetFileId);
		if (sourceBackupFileId != null && targetBackupFile != null){
			String targetBackupFileId = targetBackupFile.getId();
			readerFileMetadataDao.copyFileProcessingStatus(sourceBackupFileId,
					targetBackupFileId);
			readerFileMetadataDao.copyReaderFileMetadata(sourceBackupFileId,
					targetBackupFileId);
			readerFileMetadataDao.copyReaderFileMetadataDetail(sourceBackupFileId,
					targetBackupFileId);
			textViewSettingDao.copyTextViewSetting(sourceBackupFileId, targetBackupFileId);
		}
		
		// update already read
		if(!StringUtils.isNullOrEmpty(sharedRootId) || !StringUtils.isNullOrEmpty(syncRootId)) {
			boolean isAlreadyRead = true;
			if(sourceFile.getType().toLowerCase().equals(MetadataType.LINKFILE.toString().toLowerCase()) || !StringUtils.isNullOrEmpty(sourceFile.getSharedRootId())) {
				isAlreadyRead = alreadyReadDao.isAlreadyRead(userId, sourceFile.getId());
			}
			if(isAlreadyRead) {
				alreadyReadDao.updateAlreadyRead(targetFile.getId(), userId);
			}
		}
		
		// sync
		if(!syncRelationService.inCallerList()) {
			syncRelationService.sync(targetFile, SystemEvent.UPDATE_FILE);
		}
		
		return targetFile;
	}

	@Override
	public Metadata createTargetFileBySourceFile(BaseMetadata sourceFile,
			String targetParentId, String newName, String userId,
			String targetOwnerId, String sharedRootId, String syncRootId, boolean fromBackup) {
		// 1. create target file & attr
		Metadata targetFile = new Metadata();
		targetFile.setId(StringUtils.getUUID());
		targetFile.setParentId(targetParentId);
		targetFile.setName(newName == null ? sourceFile.getName() : newName
				.toLowerCase());
		targetFile.setOriginName(newName == null ? sourceFile.getOriginName()
				: newName);
		targetFile.setFolder(false);
		targetFile.setSize(sourceFile.getSize());
		targetFile.setModifiedAt(DateUtils.nowUTCTimestamp());
		targetFile.setModifiedBy(userId);
		targetFile.setNote(sourceFile.getNote());
		targetFile.setParams(sourceFile.getParams());
		targetFile.setIconBlockId(sourceFile.getIconBlockId());
		targetFile.setIconText(sourceFile.getIconText());
		targetFile.setIconTextColor(sourceFile.getIconTextColor());
		targetFile.setIconTextStyle(sourceFile.getIconTextStyle());
		targetFile.setEncrypted(sourceFile.isEncrypted());
		targetFile.setCreatedAt(DateUtils.nowUTCTimestamp());
		targetFile.setCreatedBy(userId);
		targetFile.setBlockId(sourceFile.getBlockId());
		targetFile.setType(MetadataType.NORMAL.toString());
		targetFile.setOwnerId(targetOwnerId);
		targetFile.setSharedRootId(sharedRootId);
		targetFile.setSyncRootId(syncRootId);
		if (sourceFile instanceof Metadata) {
			targetFile.setBackupCount(((Metadata)sourceFile).getBackupCount());
		}
		
		Metadata targetFolder = metadataDao.getMetadata(targetParentId);
		if(targetFolder == null) {
			throw new MetadataException(HttpStatus.TARGET_FOLDER_NOT_FOUND);
		}
		if(!StringUtils.isNullOrEmpty(targetFolder.getSyncRootId()) && targetFile.getType().equals(MetadataType.NORMAL.toString())) {
			// not allow to create a normal file in a internal sync folder
			throw new MetadataException(HttpStatus.CANT_CREATE_FILE_IN_THIS_FOLDER);
		}
		
		targetFile.setSortPriority(metadataDao.getMaxSortPriority(targetParentId)+1);
		metadataDao.createMetadata(targetFile);
		// create attr
		MetadataAttr attr = metadataAttrDao.getMetadataAttr(sourceFile.getId());
		attr.setMetadataIndexId(targetFile.getId());
		metadataAttrDao.createMetadataAttr(attr);
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.addFile(targetFile.getParentId(), targetFile.getSize());
		countService.updateAllParentCount(countManageModel); 				
		// 2. create target backup & backup attr, update parent of target backup
		//backupMetadataService.backupMetadata(targetFile, targetOwnerId,true);
		backupMetadataService.createSingleBackup(targetFile, targetOwnerId);
		// 3. copy association of file & backup
		String sourceFileId = sourceFile.getId();
		String targetFileId = targetFile.getId();
		String sourceBackupFileId = null;
		if (fromBackup){
			sourceBackupFileId = sourceFileId;
		}else{
			BackupMetadata sourceBackupMetadata = backupMetadataDao.getLatestBackup(sourceFileId);
			if (sourceBackupMetadata != null){
				sourceBackupFileId = sourceBackupMetadata.getId();
			}
		}
		BackupMetadata targetBackupFile = backupMetadataDao.getLatestBackup(
				targetFileId);
		readerFileMetadataDao.copyFileProcessingStatus(sourceFileId,
				targetFileId);
		readerFileMetadataDao
				.copyReaderFileMetadata(sourceFileId, targetFileId);
		readerFileMetadataDao.copyReaderFileMetadataDetail(sourceFileId,
				targetFileId);
		textViewSettingDao.copyTextViewSetting(sourceFileId, targetFileId);
		if (sourceBackupFileId != null && targetBackupFile != null){
			String targetBackupFileId = targetBackupFile.getId();
			readerFileMetadataDao.copyFileProcessingStatus(sourceBackupFileId,
					targetBackupFileId);
			readerFileMetadataDao.copyReaderFileMetadata(sourceBackupFileId,
					targetBackupFileId);
			readerFileMetadataDao.copyReaderFileMetadataDetail(sourceBackupFileId,
					targetBackupFileId);
			textViewSettingDao.copyTextViewSetting(sourceBackupFileId, 
					targetBackupFileId);
		}
		
		// update already read
		if(!StringUtils.isNullOrEmpty(sharedRootId) || !StringUtils.isNullOrEmpty(syncRootId)) {
			boolean isAlreadyRead = true;
			if(sourceFile.getType().toLowerCase().equals(MetadataType.LINKFILE.toString().toLowerCase()) || !StringUtils.isNullOrEmpty(sourceFile.getSharedRootId())) {
				isAlreadyRead = alreadyReadDao.isAlreadyRead(userId, sourceFile.getId());
			}
			if(isAlreadyRead) {
				alreadyReadDao.updateAlreadyRead(targetFile.getId(), userId);
			}
		}
		
		// sync
		if(!syncRelationService.inCallerList()) {
			syncRelationService.sync(targetFile, SystemEvent.COPY_FILE);
		}

		return targetFile;
	}

	@Override
	public void updateBackupCount(
			UpdateBackupCountRequest updateBackupCountRequest)
			throws MetadataException {
		if (updateBackupCountRequest.getBackupCount() <= -1) {
			throw new MetadataException(HttpStatus.BACKUP_COUNT_IS_NEGATIVE);
		}
		PathInfo pathInfo = pathFactory.parsePathInfo(updateBackupCountRequest, false, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		
		backupMetadataService.updateBackupCount(metadata, updateBackupCountRequest.getBackupCount());
	}

	@Override
	public BackupCountResponse getBackupCount(
			PathRequestBase getBackupCountRequest) throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(getBackupCountRequest, false, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		
		if(metadata == null || metadata.isFolder()) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		
		BackupCountResponse backupCountResponse = new BackupCountResponse();
		int backupCount = backupMetadataService.getBackupCount(metadata);
		backupCountResponse.setBackupCount(backupCount);

		return backupCountResponse;
	}

	@Override
	@Transactional
	public Boolean updateNote(UpdateNoteRequest updateNoteRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateNoteRequest, false, true);
		BaseMetadata metadata = metadataFactory.getFileByPathInfo(pathInfo);
		// update note
		metadata.setNote(updateNoteRequest.getNote());
		long modifiedAt = DateUtils.nowUTCTimestamp();
		if (metadata instanceof Metadata) {
			metadataDao.updateNote(metadata.getId(), metadata.getNote(),
					modifiedAt, updateNoteRequest.getUserId());
			BackupMetadata backupMetadata = backupMetadataDao
					.getLatestBackup(metadata.getId());
			if(backupMetadata != null){
				backupMetadata.setNote(updateNoteRequest.getNote());
				backupMetadataDao.updateBackupNote(backupMetadata.getId(),
						backupMetadata.getNote(), modifiedAt, updateNoteRequest.getUserId());
			}
			
		} else {
			backupMetadataDao.updateBackupNote(metadata.getId(),
					metadata.getNote(), modifiedAt, updateNoteRequest.getUserId());
		}
		//  send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.UPDATE_NOTE.toString(),
				metadata.getSharedRootId(), ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.UPDATE_NOTE, pathInfo);
		// sync
		if(!syncRelationService.inCallerList()) {
			syncRelationService.sync(updateNoteRequest, metadata, SystemEvent.UPDATE_NOTE);
		}
		return true;
	}

	@Override
	public NoteResponse getNote(PathRequestBase getNoteRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(getNoteRequest, false, true);
		BaseMetadata metadata = metadataFactory.getFileByPathInfo(pathInfo);
		NoteResponse noteResponse = new NoteResponse();
		noteResponse.setNote(metadata.getNote());
		return noteResponse;
	}

	@Override
	public List<FileResponse> getBackupInfo(
			PathRequestBase getBackupInfoRequest) throws MetadataException {

		PathInfo pathInfo = pathFactory.parsePathInfo(getBackupInfoRequest, false, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());

		if (metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		String backupMetadataId = metadata.getId();

		List<BackupMetadata> backupMetadatas = backupMetadataDao.getBackupByOriginalId(backupMetadataId);
		if (backupMetadatas.isEmpty()){
			return null;
		}
		
		BaseMetadata folder = metadataFactory.getBaseMetadataById(backupMetadatas.get(0).getParentId(), true);

		return metadataFactory.toFileResponses(getBackupInfoRequest.getToken(), backupMetadatas,
				false, metadataFactory.getMetadataOriginalPath(folder));


	}

	@Override
	public GetReaderFileMetadataResponse getReaderFileMetadata(
			PathRequestBase getReaderFileMetadataRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory
				.parsePathInfo(getReaderFileMetadataRequest, false, true);
		BaseMetadata metadata = metadataFactory.getFileByPathInfo(pathInfo);
		if (metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		boolean fromBackup = pathInfo.isUnderMyBackupData();
		ReaderFileMetadata readerFileMetadata = null;
		List<ReaderPageDetail> readerPageDetails = null;
		if (fromBackup) {
			readerFileMetadata = readerFileMetadataDao.getReaderBackupFileMetadata(metadata.getId());
		}else {
			readerFileMetadata = readerFileMetadataDao.getReaderFileMetadata(metadata.getId());
		}
		
		if (null == readerFileMetadata) {
			return null;
		}
		
		readerPageDetails = readerFileMetadataDao.getReaderPageDetails(metadata.getId());
		readerFileMetadata.setBackup(fromBackup);
		return GetReaderFileMetadataResponse.parse(readerFileMetadata, readerPageDetails);
	}

	@Override
	public Boolean updateReaderFileMetadata(
			UpdateReaderFileMetadataRequest updateReaderFileMetadataRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory
				.parsePathInfo(updateReaderFileMetadataRequest, false, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		ReaderFileMetadata readerFileMetadata = ReaderFileMetadata
				.parse(updateReaderFileMetadataRequest);
		readerFileMetadata.setMetadataIndexId(metadata.getId());
		readerFileMetadataDao.replaceReaderFileMetadata(readerFileMetadata);
		readerFileMetadataDao.replaceReaderPageDetail(metadata.getId(),
				updateReaderFileMetadataRequest.getPageDetails());
		// backup
		BackupMetadata backupMetadata = backupMetadataDao
				.getLatestBackup(metadata.getId());
		if (backupMetadata != null) {
			readerFileMetadata.setMetadataIndexId(backupMetadata.getId());
			readerFileMetadata.setBackup(true);
			readerFileMetadataDao.replaceReaderFileMetadata(readerFileMetadata);
			readerFileMetadataDao.replaceReaderPageDetail(backupMetadata.getId(),
					updateReaderFileMetadataRequest.getPageDetails());
		}
		return true;
	}

	@Override
	public Boolean updateFileProcessingStatus(
			UpdateFileProcessingStatusRequest updateFileProcessingStatusRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateFileProcessingStatusRequest, false, true);
		BaseMetadata metadata = updateFileProcessingStatusRequest.isFolder()? metadataFactory.getFolderByPathInfo(pathInfo) : metadataFactory.getFileByPathInfo(pathInfo);
		FileProcessingStatus fileProcessingStatus;
		if (updateFileProcessingStatusRequest.getProcessingStatus() == ProcessingStatus.PROCESSING || 
				updateFileProcessingStatusRequest.getProcessingStatus() == ProcessingStatus.LOCKING) {
			fileProcessingStatus = fileProcessingStatusDao.getFileProcessingStatus(metadata.getId());
			if (fileProcessingStatus != null && fileProcessingStatus.getProcessingStatus()  == ProcessingStatus.CANCEL) {
				if (updateFileProcessingStatusRequest.getProcessAction() == ProcessAction.ZIP) {
					deleteSingleFile((Metadata)metadata);
				} else if (updateFileProcessingStatusRequest.getProcessAction() == ProcessAction.UNZIP) {
					DeleteFolderRequest deleteFolderRequest = new DeleteFolderRequest();
					deleteFolderRequest.setPath(updateFileProcessingStatusRequest.getPath());
					deleteFolderRequest.setUserId(updateFileProcessingStatusRequest.getUserId());
					folderService.deleteFolder(deleteFolderRequest);
				}
				fileProcessingStatus = new FileProcessingStatus(
						metadata.getId(), ProcessingStatus.FAILED, 0, "", fileProcessingStatus.getProcessAction());	
				fileProcessingStatusService.updateFileProcessingStatus(fileProcessingStatus);
				throw new MetadataException(HttpStatus.PROCESS_CANCELED);
			}
		}
		fileProcessingStatus = new FileProcessingStatus(
				metadata.getId(), 
				updateFileProcessingStatusRequest.getProcessingStatus(), 
				updateFileProcessingStatusRequest.getPercentage(),
				updateFileProcessingStatusRequest.getItemInProgress(),
				updateFileProcessingStatusRequest.getProcessAction());
		fileProcessingStatusService.updateFileProcessingStatus(fileProcessingStatus);
		
		// history, xmpp and sync unnecessary !!
//		//  send dynamoDB history
//		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata.getSharedRootId());
//		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
//				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
//				DateUtils.nowUTCDateTime(),
//				ChangeReason.UPDATE_PROCESSING_STATUS.toString(),
//				metadata.getSharedRootId(), ActivityHistoryLevel.NORMAL.toString());
//		// send XMPP
//		xmppService.sendToAllDevices(SystemEvent.UPDATE_FILE_PROCESSING_STATUS, pathInfo);
//		// sync
//		if(!syncRelationService.isCaller()) {
//			syncRelationService.sync(updateFileProcessingStatusRequest, metadata, SystemEvent.UPDATE_FILE_PROCESSING_STATUS);
//		}
		return true;
	}

	@Override
	public List<BulkCreateFileResponse> bulkCreateFile(
			BulkCreateFileRequest bulkCreateFileRequestList)
			throws MetadataException {
		List<BulkCreateFileResponse> bulkCreateFileResponses = new ArrayList<BulkCreateFileResponse>();
		for (UpdateFileRequest updateFileRequest : bulkCreateFileRequestList) {
			BulkCreateFileResponse bulkCreateFileResponse = new BulkCreateFileResponse();
			try {
				updateFileRequest.setUserId(bulkCreateFileRequestList
						.getUserId());
				FileResponse fileResponse = createFile(updateFileRequest);
				bulkCreateFileResponse.setPath(fileResponse.getPath());
				bulkCreateFileResponse.setOwnerId(fileResponse.getOwnerId());
				bulkCreateFileResponse.setModifiedAt(fileResponse
						.getModifiedAt());
				bulkCreateFileResponse.setType(fileResponse.getType()
						.toString());
				bulkCreateFileResponse.setSucceed(true);
			} catch (MetadataException e) {
				String lowerCasePath = updateFileRequest.getPath()
						.toLowerCase();
				String type = null;
				if (lowerCasePath.equals(CommonFolders.MY_DEVICE_SYNC_FOLDERS
						.toString())
						|| lowerCasePath
								.startsWith(CommonFolders.MY_DEVICE_SYNC_FOLDERS
										.toString().concat("/"))) {
					type = MetadataType.LINKFILE.toString();
				} else {
					type = MetadataType.NORMAL.toString();
				}
				bulkCreateFileResponse.setPath(updateFileRequest.getPath());
				bulkCreateFileResponse.setOwnerId(updateFileRequest
						.getOwnerId());
				bulkCreateFileResponse.setModifiedAt(DateUtils
						.nowUTCTimestamp());
				bulkCreateFileResponse.setType(type);
				bulkCreateFileResponse.setSucceed(false);
			}
			bulkCreateFileResponses.add(bulkCreateFileResponse);
		}
		return bulkCreateFileResponses;
	}

	@Override
	public GetFileProcessingStatusResponse getFileProcessingStatus(
			GetFileProcessingStatusRequest getFileProcessingStatusRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(getFileProcessingStatusRequest, false, true);
		BaseMetadata metadata = getFileProcessingStatusRequest.isFolder()? metadataFactory.getFolderByPathInfo(pathInfo) : metadataFactory.getFileByPathInfo(pathInfo);
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao.getFileProcessingStatus(metadata.getId());
		if (fileProcessingStatus == null) {
			throw new MetadataException(HttpStatus.STATUS_NOT_EXIST);
		}
		GetFileProcessingStatusResponse response = new GetFileProcessingStatusResponse();
		boolean canCancel = fileProcessingStatus.getProcessingStatus() == ProcessingStatus.PROCESSING || 
				fileProcessingStatus.getProcessingStatus() == ProcessingStatus.WAITING ? true : false;
		response.setCanCancel(canCancel);			
		return response.fromFileProcessingStatus(fileProcessingStatus);
	}

	@Override
	public void encryptFile(EncryptRequest encryptRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(encryptRequest, false, true);
		BaseMetadata metadata = metadataFactory.getFileByPathInfo(pathInfo);
		// check: encryption rule
		checkCanEncryptFile(metadata);
		// check: path is under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao.getFileProcessingStatus(metadata.getId());
		if (fileProcessingStatus != null && fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}
		// update processing status
		fileProcessingStatus = new FileProcessingStatus(metadata.getId(), ProcessingStatus.WAITING, 0, "", ProcessAction.ENCRYPT);
		fileProcessingStatusDao.updateFileProcessingStatus(fileProcessingStatus);
		// send encrypt MQ
		mqService.sendForEncrypt(metadata.isFolder(), encryptRequest);
		// send mail
		if (encryptRequest.isSendNotification()) {
			emailService.sendEncryptionNotificationMail(encryptRequest);
		}
		//  send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.ENCRYPT_FILE.toString(),
				metadata.getSharedRootId(), ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.ENCRYPT_FILE, pathInfo);
						
	}
	
	private void checkCanEncryptFile(BaseMetadata metadata) {
		String path = metadataFactory.getMetadataOriginalPath(metadata).toLowerCase();
		for (CommonFolders commonFolder : CommonFolders.values()) {
			if (commonFolder != CommonFolders.MY_SHARED_FOLDERS 
					&& commonFolder != CommonFolders.My_STORAGE_DATA
					&& path.startsWith(commonFolder.toString().concat("/"))) {
				throw new MetadataException(HttpStatus.CANT_ENCRYPT);
			}
		}
	}
	
	@Override
	@Transactional
	public void afterEncrypt(UpdateFileRequest updateFileRequest) {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateFileRequest, false, true);
		Metadata metadata = (Metadata)metadataFactory.getFileByPathInfo(pathInfo);
		// delete original file 
		metadataDao.deleteMetadata(metadata.getId());
		metadataAttrService.deleteMetadataAttr(metadata.getId());
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.deleteFile(metadata.getParentId(), metadata.getSize());
		countService.updateAllParentCount(countManageModel); 
		// delete original backups
		List<BackupMetadata> backups = backupMetadataDao.getBackupByOriginalId(metadata.getId());
		if (backups != null) {
			List<String> ids = metadataFactory.getIds(backups);
			backupMetadataDao.deleteBackupByIds(ids);
			// delete attr
			metadataAttrService.deleteMetadataAttrs(ids);
			// update backup parents' size/count
			CountManageModel countManageModelBackup = new CountManageModel();
			for (BackupMetadata backup : backups) {
				countManageModel.deleteFile(backup.getParentId(), backup.getSize());
			}
			countService.updateAllParentCountBackup(countManageModelBackup); 
		}
		// create encrypted file w/o backup
		updateFileRequest.setNote(metadata.getNote());
		createFile(updateFileRequest);
	}

	@Override
	public void decryptFile(EncryptRequest decryptRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(decryptRequest, false, true);
		BaseMetadata metadata = metadataFactory.getFileByPathInfo(pathInfo);
		if (metadata.getBlockId().length() == 96) {
			decryptFileOld(decryptRequest);
			return;
		}
		// check can decrypt file
		checkCanDecryptFile(metadata);
		// check: path is under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao.getFileProcessingStatus(metadata.getId());
		if (fileProcessingStatus != null && fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}
		// check password
		gatewayService.checkPassword(decryptRequest.getToken(), decryptRequest.getPassword(), metadata.getBlockId());
		// update processing status
		fileProcessingStatus = new FileProcessingStatus(metadata.getId(), ProcessingStatus.WAITING, 0, "", ProcessAction.DECRYPT);
		fileProcessingStatusDao.updateFileProcessingStatus(fileProcessingStatus);
		// send decrypt MQ
		mqService.sendForDecrypt(decryptRequest);
		
		//  send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.DECRYPT_FILE.toString(),
				metadata.getSharedRootId(), ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.DECRYPT_FILE, pathInfo);
	}

	private void checkCanDecryptFile(BaseMetadata metadata) {
		if (metadata.isEncrypted() == false) {
			throw new MetadataException(HttpStatus.CANT_DECRYPT);
		}
	}
	
	@Override
	@Transactional
	public void afterDecrypt(UpdateFileRequest updateFileRequest) {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateFileRequest, false, true);
		Metadata metadata = (Metadata)metadataFactory.getFileByPathInfo(pathInfo);
		// delete encrypted file 
		metadataDao.deleteMetadata(metadata.getId());
		// delete attr
		metadataAttrService.deleteMetadataAttr(metadata.getId());
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.deleteFile(metadata.getParentId(), metadata.getSize());
		countService.updateAllParentCount(countManageModel); 
		// create file 
		updateFileRequest.setNote(metadata.getNote());
		createFile(updateFileRequest);
	}

	@Override
	@Transactional
	public void encryptFileOld(EncryptRequest encryptRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(encryptRequest, false, true);
		BaseMetadata metadata = metadataFactory.getFileByPathInfo(pathInfo);
		String newBlockId = gatewayService.encryptFile(encryptRequest.getToken(), encryptRequest.getPassword(), metadata.getBlockId());
		long modifiedAt = DateUtils.nowUTCTimestamp();
		metadataDao.updateFileEncryption(metadata.getId(), newBlockId, true, modifiedAt, encryptRequest.getUserId());
		BackupMetadata backupMetadata = backupMetadataDao.getLatestBackup(metadata.getId());
		if (backupMetadata != null) {
			backupMetadataDao.updateBackupFileEncryption(backupMetadata.getId(), newBlockId, true, modifiedAt, encryptRequest.getUserId());
		}
	}
	
	@Override
	@Transactional
	public void decryptFileOld(EncryptRequest decryptRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(decryptRequest, false, true);
		BaseMetadata metadata = metadataFactory.getFileByPathInfo(pathInfo);
		String newBlockId = gatewayService.decryptFile(decryptRequest.getToken(), decryptRequest.getPassword(), metadata.getBlockId());
		long modifiedAt = DateUtils.nowUTCTimestamp();
		metadataDao.updateFileEncryption(metadata.getId(), newBlockId, false, modifiedAt, decryptRequest.getUserId());
		BackupMetadata backupMetadata = backupMetadataDao.getLatestBackup(metadata.getId());
		if (backupMetadata != null) {
			backupMetadataDao.updateBackupFileEncryption(backupMetadata.getId(), newBlockId, false, modifiedAt, decryptRequest.getUserId());
		}
	}

	@Override
	public List<ExistFile> existFile(ExistFileRequest existFileRequest)
			throws MetadataException {
		List<ExistFile> metadatas = new ArrayList<ExistFile>();
		for (ExistFile item : existFileRequest.getItems()) {
			PathRequestBase pathRequestBase = new PathRequestBase();
			pathRequestBase.setPath(item.getPath());
			pathRequestBase.setSourcePath(item.getSourcePath());
			pathRequestBase.setOwnerId(item.getOwnerId());
			pathRequestBase.setSharedRootId(item.getSharedRootId());
			pathRequestBase.setUserId(existFileRequest.getUserId());
			pathRequestBase.setToken(existFileRequest.getToken());
			pathRequestBase.setDeviceId(existFileRequest.getDeviceId());
			PathInfo pathInfo = pathFactory.parsePathInfo(pathRequestBase, false, true);
			Metadata metadata = metadataDao.getMetadataByPath(
					pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
					pathInfo.getFullSourcePath());
			boolean searchResultIncludeFolder = metadata != null ? true : false;
			boolean searchResultExcludeFolder = metadata != null
					&& metadata.isFolder() == false ? true : false;
			if (existFileRequest.isIncludeFolder() == true) {
				item.setExisted(searchResultIncludeFolder);
			} else {
				item.setExisted(searchResultExcludeFolder);
			}
			metadatas.add(item);
		}
		return metadatas;
	}

	@Override
	@Transactional
	public void updateImage(UpdateImageRequest updateImageRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateImageRequest, false, true);
		BaseMetadata metadata = metadataFactory.getBaseMetadataByPath(pathInfo);
		if (metadata == null || metadata.isBackup()) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		if (updateImageRequest.getWidth() != null
				&& updateImageRequest.getHeight() != null) {
			metadata.setWidth(updateImageRequest.getWidth());
			metadata.setHeight(updateImageRequest.getHeight());
			metadataDao.updateImageWidthHeight(metadata.getId(),
					metadata.getWidth(), metadata.getHeight());
		}
		if (updateImageRequest.getSize() != null) {
			long deltaSize = updateImageRequest.getSize() - metadata.getSize();
			if (deltaSize != 0) {
				metadata.setSize(updateImageRequest.getSize());
				metadataDao.updateImageSize(metadata.getId(), metadata.getSize());
				metadataAttrDao.updateMetadataAttrOnOrigin(new MetadataAttr(metadata.getId(), false, 0, 0, deltaSize,0,0,0,0,0));
				// update parents' size
				CountManageModel countManageModel = new CountManageModel();
				countManageModel.updateFile(metadata.getParentId(), deltaSize);
				countService.updateAllParentCount(countManageModel); 
			}
		}
		if (updateImageRequest.getBlockId() != null) {
			metadata.setBlockId(updateImageRequest.getBlockId());
			metadataDao.updateImageBlockId(metadata.getId(),
					metadata.getBlockId());
		}
		if (updateImageRequest.getModifiedAt() != null) {
			metadata.setModifiedAt(updateImageRequest.getModifiedAt());
			metadataDao.updateImageModifiedAt(metadata.getId(),
					metadata.getModifiedAt());
		}

		// only takes a backup when updateImageRequest.createBackup is set to
		// true
		if (updateImageRequest.getCreateBackup() == true) {
			backupMetadataService.backupMetadata((Metadata) metadata, updateImageRequest.getUserId());
		}
		
		//  send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.UPDATE_IMAGE.toString(),
				metadata.getSharedRootId(), ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.UPDATE_IMAGE, pathInfo);
		
		// sync
		if(!syncRelationService.isCaller()) {
			syncRelationService.sync(updateImageRequest, metadata, SystemEvent.UPDATE_IMAGE);
		}
	}

	
	@Override
	public GetBulkFileInfoResponse getBulkInfoByBlockId(
			GetBulkFileInfoRequest getBulkFileInfoRequest)
			throws MetadataException {
		
		List<FileResponse> files = new ArrayList<FileResponse>();
		GetBulkFileInfoResponse getBulkFileInfoResponse = new GetBulkFileInfoResponse();
		
		HashMap<String, String> mapDeviceName = new HashMap<String, String>();
		GetAllDeviceRequest request = new GetAllDeviceRequest();
		request.setUserId(getBulkFileInfoRequest.getUserId());
		request.setToken(getBulkFileInfoRequest.getToken());
		List<GetDeviceResponse> devices = deviceService.getAllDevice(request).getDevices();
		for(GetDeviceResponse device : devices) {
			mapDeviceName.put(device.getDeviceUid(), device.getName());
		}

		for(int i = 0; i < getBulkFileInfoRequest.getBlocks().size(); i++) {
			BulkFileInfoItem bulkFileInfoItem = getBulkFileInfoRequest.getBlocks().get(i);
			List<BaseMetadata> metadatas = metadataDao.getOwnerMetadatasByBlockId(getBulkFileInfoRequest.getUserId(), bulkFileInfoItem.getBlock());
			List<? extends BaseMetadata> backupMetadata = backupMetadataDao.getBackupMetadatasByBlockId(getBulkFileInfoRequest.getUserId(), bulkFileInfoItem.getBlock());
			metadatas.addAll(backupMetadata);
			for(BaseMetadata item : metadatas){
				if(item == null) continue;
				if(!item.getSharedRootId().equals("")){
					try {
						Boolean canAccess = collaborateService.canAccessCollaborate(item.getSharedRootId(), item.getOwnerId(), 
								getBulkFileInfoRequest.getUserId(), getBulkFileInfoRequest.getPath());
						if(!canAccess) continue;
					}
					catch (Exception ex) {
						continue;
					}
				}
				Map<String, Object> appendOtherParams = new HashMap<String, Object>();
				appendOtherParams.put("block_id", bulkFileInfoItem.getBlock());
				appendOtherParams.put("score", bulkFileInfoItem.getScore());
				appendOtherParams.put("highlights", bulkFileInfoItem.getHighlights());
				
				// process file by path
				// only owner can search sharedfolder's content
				if(!item.isFolder() && getBulkFileInfoRequest.getUserId().equals(item.getOwnerId())) {
					FileResponse response = metadataFactory.toFileResponse(item, true, appendOtherParams);
					response.setDeviceName(mapDeviceName.get(item.getDeviceUid()));
					if(StringUtils.isNullOrEmpty(getBulkFileInfoRequest.getPath()) ||
							response.getPath().toLowerCase().startsWith(getBulkFileInfoRequest.getPath().concat("/").toLowerCase())){
						files.add(response);
					}							
				}
			}			
		}
		getBulkFileInfoResponse.setTotalPages(null);
		getBulkFileInfoResponse.setFiles(files);
		return getBulkFileInfoResponse;
	}
	
	@Override
	public void bulkUpdateLinkFile(BulkActionRequest<BulkActionRequestItem> bulkActionRequest) throws MetadataException {
		for (int i = 0; i < bulkActionRequest.size(); i++) {
			try {
				BulkActionRequestItem item = bulkActionRequest.get(i);
				
				if(StringUtils.isNullOrEmpty(item.getPath()) || StringUtils.isNullOrEmpty(item.getSourcePath())) {
					throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
				}
				
				PathRequestBase request = new PathRequestBase();
				request.fromRequestBase(bulkActionRequest);
				request.setPath(item.getPath());
				request.setSourcePath(item.getSourcePath());
				
				updateLinkFile(request);
				
			} catch (MetadataException me) {
				throw new MetadataException(me.getHttpStatus(),
						bulkActionRequest.toErrorResponse(i));
			} catch (Exception ex) {
				throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR,
						bulkActionRequest.toErrorResponse(i));
			}
		}
	}
	
	@Override
	public TextViewSetting getTextViewSetting(GetTextViewSettingRequest getTextViewSettingRequest) throws MetadataException
	{
		PathInfo pathInfo = pathFactory.parsePathInfo(getTextViewSettingRequest, false, true);
		BaseMetadata metadata;
		if (pathInfo.isUnderMyBackupData()) {
			metadata = backupMetadataDao.getBackupMetadataByPath(pathInfo.generateBackupFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath(), false);
		} else {
			metadata = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		}
		
		if(metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if(metadata.isFolder()) {
			throw new MetadataException(HttpStatus.TARGET_PATH_IS_FOLDER);
		}
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		TextViewSetting textViewSetting = textViewSettingDao.getTextViewSetting(
				metadata.getId(), getTextViewSettingRequest.getUserId(), getTextViewSettingRequest.getViewerDeviceUniqueId());
		
		if(textViewSetting == null) {
			textViewSetting = new TextViewSetting();
			textViewSetting.setMetadataIndexId(metadata.getId());
			textViewSetting.setViewerId(getTextViewSettingRequest.getUserId());
			textViewSetting.setViewerDeviceUniqueId(getTextViewSettingRequest.getViewerDeviceUniqueId());
		}
		
		textViewSetting.setMetadataIndexId(null);
		
		return textViewSetting;
	}
	
	@Override
	public void updateTextViewSetting(UpdateTextViewSettingRequest updateTextViewSettingRequest) throws MetadataException
	{
		PathInfo pathInfo = pathFactory.parsePathInfo(updateTextViewSettingRequest, false, true);
		BaseMetadata metadata;
		if (pathInfo.isUnderMyBackupData()) {
			metadata = backupMetadataDao.getBackupMetadataByPath(pathInfo.generateBackupFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath(), false);
		} else {
			metadata = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		}
		
		if(metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if(metadata.isFolder()) {
			throw new MetadataException(HttpStatus.TARGET_PATH_IS_FOLDER);
		}
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.ENCRYPTED);
		}
		TextViewSetting textViewSetting = updateTextViewSettingRequest.toTextViewSetting();
		textViewSetting.setMetadataIndexId(metadata.getId());
		
		textViewSettingDao.updateTextViewSetting(textViewSetting);
		textViewSettingDao.updateAllBackupsTextViewSetting(textViewSetting);
	}
	
	@Override
	public void updateAlreadyRead(PathRequestBase pathRequestBase) {
		PathInfo pathInfo = pathFactory.parsePathInfo(pathRequestBase, false, true);
		Metadata metadata = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		if (metadata == null){
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (!metadata.getType().equals(MetadataType.LINKFILE.toString()) && metadata.getSharedRootId().equals("") && metadata.getSyncRootId().equals("")){
			throw new MetadataException(HttpStatus.FILE_NOT_ALLOW_ACCESS);
		}
		alreadyReadDao.updateAlreadyRead(metadata.getId(),pathRequestBase.getUserId());
		//  create active history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(pathRequestBase.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				String.valueOf(DateUtils.nowUTCDateTime()),
				ChangeReason.ALREADY_READ.toString(),
				metadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
	}
	
	@Override
	public AlreadyReadResponse getAlreadyRead(
			PathRequestBase pathRequestBase) {
		PathInfo pathInfo = pathFactory.parsePathInfo(pathRequestBase, false, true);
		Metadata metadata = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		if (metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		List<AlreadyRead> alreadyreads = alreadyReadDao.getAlreadyRead(metadata.getId());
		
		AlreadyReadResponse alreadyReadResponse = new AlreadyReadResponse();
		List<String> read = new ArrayList<String>();
		List<String> unread = new ArrayList<String>();
		
		List<CollaborateMember> memberInfo = collaborateDao.getCollaborateMembersByMetadataIndexId(pathRequestBase.getSharedRootId());
		List<CellPhone> cellphones = new ArrayList<CellPhone>();
		for(CollaborateMember member : memberInfo) {
			cellphones.addAll(member.getCellphones());
		}
		
		Map<String, String> cellphoneDisplayNameMap = collaborateService.findCellphoneDisplayNameMap(cellphones);
		Map<String, String> cellphoneUserIdMap = ssoService.findCellphoneUserIdMap(cellphones);
		String ownerDisplayName = ssoService.findDisplayNameByUserId(pathInfo.getOwnerId());
		
		boolean ownerread = false;
		for (AlreadyRead alreadyread : alreadyreads){
			if (pathInfo.getOwnerId().equals(alreadyread.getUserId())){
				ownerread = true;
				read.add(ownerDisplayName);
				break;
			}
		}
		if (!ownerread){
			unread.add(ownerDisplayName);
		}
		
		if (!memberInfo.isEmpty()){
			for (CollaborateMember member : memberInfo){
				boolean findRead = false;
				for(CellPhone cellphone : member.getCellphones()) {
					String cellphoneString = cellphone.toString(); 
					if(cellphoneDisplayNameMap.containsKey(cellphoneString)) {
						member.setDisplayName(cellphoneDisplayNameMap.get(cellphoneString));
						break;
					}
				}
		    	if (StringUtils.isNullOrEmpty(member.getDisplayName())){
		    		member.setDisplayName(member.getNickname());
		    	}
		    	if (!alreadyreads.isEmpty()){
					for (AlreadyRead alreadyread : alreadyreads){
						for(CellPhone cellphone : member.getCellphones()) {
							String cellphoneString = cellphone.toString();
							String memberUserId = cellphoneUserIdMap.get(cellphoneString);
							if(!StringUtils.isNullOrEmpty(memberUserId) &&
									memberUserId.equals(alreadyread.getUserId())) {
								read.add(member.getDisplayName());
						    	findRead = true;
						    	break;
							}
						}
						if (findRead){
							break;
						}
					}
		    	}
				if (!findRead){
					unread.add(member.getDisplayName());
				}
			}
		}

		alreadyReadResponse.setRead(read);
		alreadyReadResponse.setUnread(unread);
		return alreadyReadResponse;
	}
	
	@Override
	@Transactional
	public List<FileResponse> updateAlreadyReadAndCount(String userId, BaseMetadata metadata, List<FileResponse> fileResponses, boolean isFolder) {
        if (metadata != null && 
        		(isFolder && (!"".equals(metadata.getSharedRootId()) || 
        				!"".equals(metadata.getSyncRootId()) || 
        				metadata.getType().equals(MetadataType.COMMON.toString()))) ||
				(!isFolder && (!"".equals(metadata.getSharedRootId()) || 
						metadata.getType().equals(MetadataType.LINKFILE.toString())))
        	){
        	List<String> metadataIndexIdList = new ArrayList<String>();
        	for (FileResponse fileResponse : fileResponses){
        		metadataIndexIdList.add(fileResponse.getId());
        	}
        	if (!metadataIndexIdList.isEmpty()){
	        	List<AlreadyRead> readResults = alreadyReadDao.batchGetAlreadyRead(metadataIndexIdList);
	        	for (FileResponse fileResponse : fileResponses){
	        		for (AlreadyRead readResult : readResults){
	        			if (fileResponse.getId().equals(readResult.getMetadataIndexId())){
	        				fileResponse.setAlreadyReadCount(fileResponse.getAlreadyReadCount()+1);
	        				if (readResult.getUserId().equals(userId)){
	        					fileResponse.setAlreadyRead(true);
	        				}
	        			}
	        		}
	        	}
        	}
        }else{
        	for (FileResponse fileResponse : fileResponses){
        		fileResponse.setAlreadyRead(true);
        		fileResponse.setAlreadyReadCount(1);
        	}
        }
        return fileResponses;
	}
	
	private FileResponse updateAlreadyReadAndCount(String userId, BaseMetadata metadata, FileResponse fileResponse) {
		List<FileResponse> fileResponses = new ArrayList<FileResponse>();
		fileResponses.add(fileResponse);
		return updateAlreadyReadAndCount(userId, metadata, fileResponses,false).get(0);
	}
	
	@Override
	public List<GetExtensionBackupResponse> getExtensionBackup(
			RequestBase getExtensionBackupRequest) throws MetadataException {
		List<DefaultBackupSetting> defaultExtensionBackups = 
				extensionBackupDao.getExtensionBackup(getExtensionBackupRequest.getUserId());
		List<GetExtensionBackupResponse> extensionBackups = new ArrayList<GetExtensionBackupResponse>();
		for(DefaultBackupSetting extensionBackup : defaultExtensionBackups){
			extensionBackups.add(extensionBackup.toGetExtensionBackupResponse());
		}
		return extensionBackups;
	}
	
	@Override
	@Transactional
	public void addExtensionBackup(
			AddExtensionBackupRequest addExtensionBackupRequest) {
		List<DefaultBackupSetting> defaultExtensionBackups = 
				extensionBackupDao.getExtensionBackup(addExtensionBackupRequest.getUserId());
		for(DefaultBackupSetting defaultExtensionBackup : defaultExtensionBackups){
            if (defaultExtensionBackup.getExtension().equals(addExtensionBackupRequest.getExtension()) ){
				throw new MetadataException(HttpStatus.FILE_EXIST);
			}
		}
		extensionBackupDao.addExtensionBackup(addExtensionBackupRequest.getUserId(),addExtensionBackupRequest.getExtension());
	}
	
	private void addExtensionBackupByCreateFile(String ownerId, String path){
		String extension = FilenameUtils.getExtension(path);
		if (!"".equals(extension)){
			List<DefaultBackupSetting> defaultBackupSettings = 
					extensionBackupDao.getExtensionBackup(ownerId);
			boolean findExtension = false;
			for (DefaultBackupSetting defaultBackupSetting : defaultBackupSettings){
				if (defaultBackupSetting.getExtension().equals(extension)){
					findExtension = true;
				}
			}
			if (!findExtension){
				extensionBackupDao.addExtensionBackup(ownerId,extension);
			}
		}
	}
	
	@Override
	@Transactional
	public void deleteExtensionBackup(
			DeleteExtensionBackupRequest deleteExtensionBackupRequest) {
		List<Integer> deleteIds = new ArrayList<Integer>();
		List<DefaultBackupSetting> defaultExtensionBackups = 
				extensionBackupDao.getExtensionBackup(deleteExtensionBackupRequest.getUserId());
		for(DefaultBackupSetting defaultExtensionBackup : defaultExtensionBackups){
			if (deleteExtensionBackupRequest.getId() != null && !deleteExtensionBackupRequest.getId().isEmpty()){
				for(Integer deleteIdRequest : deleteExtensionBackupRequest.getId()){
					if (deleteIdRequest.intValue() != defaultExtensionBackup.getId()){
				    	deleteIds.add(deleteIdRequest);
				    }
				}
			}
		}
		if (!deleteIds.isEmpty()){
		    extensionBackupDao.deleteExtensionBackup(deleteExtensionBackupRequest.getUserId(),deleteIds);
		}

	}

	@Override
	public void setExtensionBackup(RequestBase requestBase,
			List<DefaultBackupSetting> updateExtensionBackupRequest) {
		for (DefaultBackupSetting updateExtension : updateExtensionBackupRequest){
			updateExtension.setUser(requestBase.getUserId());
		}
		List<DefaultBackupSetting> defaultBackupSettings = extensionBackupDao.getExtensionBackup(requestBase.getUserId());
		List<String> effectExtensions = new ArrayList<String>();
		List<DefaultBackupSetting> effectSetting = new ArrayList<DefaultBackupSetting>();
		for (DefaultBackupSetting updateExtension : updateExtensionBackupRequest){
			for (DefaultBackupSetting defaultBackupSetting : defaultBackupSettings){
				if (updateExtension.getId() == defaultBackupSetting.getId() &&
						updateExtension.getBackupCount() != defaultBackupSetting.getBackupCount()){
					DefaultBackupSetting setting = updateExtension;
					setting.setUser(requestBase.getUserId());
					setting.setExtension(defaultBackupSetting.getExtension());
					effectExtensions.add(setting.getExtension());
					effectSetting.add(setting);
				}
			}
		}
		// change sub files of same extension backup count and unbackup
		if (!effectSetting.isEmpty()){
			List<Metadata> allExtensionMetadata = metadataDao.getOwnerMetadatasByExtensions(requestBase.getUserId(), effectExtensions);
			List<Metadata> resetBackupCountMetadatas = new ArrayList<Metadata>();
			HashMap<String, Integer> needUnbackupMetadata = new HashMap<String, Integer>();
			if (!allExtensionMetadata.isEmpty()){
				for (Metadata metadata : allExtensionMetadata){
					if (metadata.getBackupCount() != -1){
						metadata.setBackupCount(-1);
						resetBackupCountMetadatas.add(metadata);
					}
					String extension = FilenameUtils.getExtension(metadata.getName());
					for (DefaultBackupSetting effectExtension : effectSetting){
						if (extension.equals(effectExtension.getExtension())){
							needUnbackupMetadata.put(metadata.getId(), effectExtension.getBackupCount());
						}
					}
				}
				if (!resetBackupCountMetadatas.isEmpty()){
				    metadataDao.batchResetBackupCount(resetBackupCountMetadatas);
				}
			}
			
			if (!needUnbackupMetadata.isEmpty()){
				List<BackupMetadata> unbackupBackupMetadatas = new ArrayList<BackupMetadata>();
				List<BackupMetadata> backups = backupMetadataDao.getBackups(new ArrayList<String>(needUnbackupMetadata.keySet()));
				for (BackupMetadata backup : backups){
					if (needUnbackupMetadata.keySet().contains(backup.getOriginalIndexId()) &&
							backup.getBackupNo() > needUnbackupMetadata.get(backup.getOriginalIndexId())){
						unbackupBackupMetadatas.add(backup);
					}
				}
				if (!unbackupBackupMetadatas.isEmpty()){
				    backupMetadataDao.batchUnbackup(unbackupBackupMetadatas);
				}
			}
			
			extensionBackupDao.batchupdateExtensionBackup(effectSetting);

		}
	}
	
	@Transactional
	private void updateLinkFile(PathRequestBase pathRequest) {
		PathInfo pathInfo = pathFactory.parsePathInfo(pathRequest, false, true);
		Metadata metadata = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), "", "");
		
		if(metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if(metadata.isFolder()) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		
		Metadata targetFile = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		if(targetFile != null) {
			throw new MetadataException(HttpStatus.FILE_EXIST);
		}
		
		// update metadata
		metadataDao.updateLinkFile(metadata.getId(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		
		// update backup
		BackupMetadata backup = backupMetadataDao.getLatestBackup(metadata.getId());
		if (backup != null) {
			backupMetadataDao.updateBackupAsLinkFile(backup.getId(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		}
	}
	
	@Override
	@Transactional
	public void restoreBackup(RestoreBackupRequest restoreBackupRequest) {
		PathInfo pathInfo = pathFactory.parsePathInfo(restoreBackupRequest, false, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (metadata == null || metadata.isFolder()){
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		String backupMetadataId = metadata.getId();

		int index = restoreBackupRequest.getName().lastIndexOf("_");
		if (index < 1){
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		String strNo = restoreBackupRequest.getName().substring(index+1);  
		index = strNo.indexOf(".");
		if (index > 0) {
			strNo = strNo.substring(0, index);
		}
		int backupNo = 0;
		try {
			backupNo = Integer.parseInt(strNo);
		} catch (RuntimeException e) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		
		List<BackupMetadata> backupMetadatas = backupMetadataDao.getBackupByOriginalId(backupMetadataId);
		for (BackupMetadata backupMetadata : backupMetadatas){
			if (backupNo == backupMetadata.getBackupNo()){
				if (backupMetadata.getBackupNo() == 1){
					return;
				}
				MetadataAttr attr = metadataAttrDao.getMetadataAttr(metadata.getId());
				attr.setTotalSize(metadata.getSize()); 
				metadataAttrDao.updateMetadataAttr(attr);
				CountManageModel countManageModel = new CountManageModel();
				countManageModel.updateFile(metadata.getId(), backupMetadata.getSize() - metadata.getSize());
				countService.updateAllParentCount(countManageModel); 

				metadata.setSize(backupMetadata.getSize());
				metadata.setModifiedAt(DateUtils.nowUTCTimestamp());
				metadata.setModifiedBy(restoreBackupRequest.getUserId());
				metadata.setNote(backupMetadata.getNote());
				metadata.setParams(backupMetadata.getParams());
				metadata.setIconBlockId(backupMetadata.getIconBlockId());
				metadata.setIconText(backupMetadata.getIconText());
				metadata.setIconTextColor(backupMetadata.getIconTextColor());
				metadata.setIconTextStyle(backupMetadata.getIconTextStyle());
				metadata.setBlockId(backupMetadata.getBlockId());
				
				metadataDao.updateFileMetadata(metadata);
				
				textViewSettingDao.copyTextViewSetting(backupMetadata.getId(), metadata.getId());

				backupMetadataService.backupMetadata(metadata, pathInfo.getUserId());
				
				// send dynamoDB history
				String targetFullOwnerPath = StringUtils.concatFilePath(metadata.getOwnerId(), 
				metadataFactory.getMetadataOriginalPath(metadata));
				String oldSharedUpperPath = metadataFactory.getSharedUpperPath(backupMetadata.getSharedRootId());
				String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata.getSharedRootId());
				dynamoDBService.createActivityHistory( targetFullOwnerPath,pathInfo.getFullOwnerPath(),sharedUpperPath,oldSharedUpperPath,
				metadata.getId(), pathInfo.getUserId(), DateUtils.nowUTCDateTime(), ChangeReason.MOVE_FILE.toString(), 
				metadata.getSharedRootId(), ActivityHistoryLevel.NORMAL.toString());
				// send XMPP
				PathInfo newPathInfo = new PathInfo();
				newPathInfo.setFullOwnerPath(targetFullOwnerPath);
				newPathInfo.setOwnerId(metadata.getOwnerId());
				newPathInfo.setSharedRootId(metadata.getSharedRootId());
				xmppService.sendToAllDevices(SystemEvent.MOVE_FILE, pathInfo, newPathInfo);				
			    return;
			}
		}
		throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
	}
}
