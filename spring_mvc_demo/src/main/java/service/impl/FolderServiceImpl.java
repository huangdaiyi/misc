package service.impl;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import model.BackupMetadata;
import model.BaseMetadata;
import model.BulkInfoItem;
import model.CountAffected;
import model.CountManageModel;
import model.FileProcessingStatus;
import model.FolderMapItem;
import model.GlobalIcon;
import model.GlobalIconGroup;
import model.ItemStyle;
import model.Metadata;
import model.MetadataAttr;
import model.MobileFolders;
import model.PageProfileProperty;
import model.PathInfo;
import model.PathRequestBase;
import model.PriorityInfo;
import model.ProfileProperty;
import model.RequestBase;
import model.SharedRootMap;
import model.SourceTargetMap;
import model.request.AddGlobalIconRequest;
import model.request.BulkActionRequest;
import model.request.BulkCreateRequestItem;
import model.request.CancelCollaborateRequest;
import model.request.CancelProcessRequest;
import model.request.CopyFolderRequest;
import model.request.CreateCollaborateRequest;
import model.request.CreateGlobalIconGroupRequest;
import model.request.DeleteFolderBackupRequest;
import model.request.DeleteFolderRequest;
import model.request.DeleteGlobalIconGroupRequest;
import model.request.DeleteGlobalIconRequest;
import model.request.EncryptRequest;
import model.request.GetAllDeviceRequest;
import model.request.GetBulkInfoRequest;
import model.request.GetFolderRequest;
import model.request.GetGlobalIconRequest;
import model.request.GetSearchResultRequest;
import model.request.LinkFoldersRequest;
import model.request.MoveFolderRequest;
import model.request.MoveGlobalIconRequest;
import model.request.ProfilePropertyRequest;
import model.request.RenameFolderRequest;
import model.request.RenameGlobalIconGroupRequest;
import model.request.ResetGlobalIconRequest;
import model.request.SetItemStyleRequest;
import model.request.SetSubItemsStyleRequest;
import model.request.SharedToSyncRequest;
import model.request.UnzipRequest;
import model.request.UpdateFileRequest;
import model.request.UpdateFolderBackupCountRequest;
import model.request.UpdateFolderNoteRequest;
import model.request.UpdateFolderRequest;
import model.request.UpdateIconRequest;
import model.request.UpdatePriorityRequest;
import model.request.ZipRequest;
import model.request.ZipSource;
import model.response.CreateCollaborateResponse;
import model.response.FileResponse;
import model.response.FolderResponse;
import model.response.GetBulkFileInfoResponse;
import model.response.GetBulkInfoResponse;
import model.response.GetDeviceResponse;
import model.response.GetFolderBackupCountResponse;
import model.response.GetFolderNoteResponse;
import model.response.GetFolderResponse;
import model.response.GetGlobalIconResponse;
import model.response.GetSubBackupCountResponse;
import model.response.LinkFolderResponse;
import model.response.LinkFoldersResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Function;
import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import service.BackupMetadataService;
import service.CollaborateService;
import service.CountService;
import service.DeviceService;
import service.DynamoDBService;
import service.EmailService;
import service.FileProcessingStatusService;
import service.FileService;
import service.FolderService;
import service.MQService;
import service.MetadataAttrService;
import service.SyncRelationService;
import service.XmppService;
import utils.DateUtils;
import utils.MathUtils;
import utils.StringUtils;
import constants.ActivityHistoryLevel;
import constants.ChangeReason;
import constants.CommonFolders;
import constants.ExtraIcon;
import constants.HttpStatus;
import constants.MetadataType;
import constants.ProcessAction;
import constants.ProcessingStatus;
import constants.SortType;
import constants.SyncType;
import constants.SystemEvent;
import controller.FolderController;
import dao.AlreadyReadDao;
import dao.BackupMetadataDao;
import dao.CollaborateDao;
import dao.FileProcessingStatusDao;
import dao.FolderDao;
import dao.MetadataAttrDao;
import dao.MetadataDao;
import dao.ProfilePropertyDao;
import dao.ReaderFileMetadataDao;
import dao.SyncRelationDao;
import dao.TextViewSettingDao;
import exception.MetadataException;
import factory.MetadataFactory;
import factory.PathFactory;

@Service
public class FolderServiceImpl implements FolderService {

	@Autowired
	private PathFactory pathFactory;
	@Autowired
	private MetadataFactory metadataFactory;
	@Autowired
	private MetadataAttrDao metadataAttrDao;
	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private BackupMetadataDao backupMetadataDao;
	@Autowired
	private FolderDao folderDao;
	@Autowired
	private ProfilePropertyDao profilePropertyDao;
	@Autowired
	private MetadataAttrService metadataAttrService;
	@Autowired
	private BackupMetadataService backupMetadataService;
	@Autowired
	private CountService countService;
	@Autowired
	private FileService fileService;
	@Autowired
	private DeviceService deviceService;
	@Autowired
	private FileProcessingStatusService fileProcessingStatusService;
	@Autowired
	private FileProcessingStatusDao fileProcessingStatusDao;
	@Autowired
	private MQService mqService;
	@Autowired
	private XmppService xmppService;
	@Autowired
	private DynamoDBService dynamoDBService;
	@Autowired
	private ReaderFileMetadataDao readerFileMetadataDao;
	@Autowired
	private SyncRelationService syncRelationService;
	@Autowired
	private SyncRelationDao syncRelationDao;
	@Autowired
	private EmailService emailService;
	@Autowired
	private CollaborateService collaborateService;
	@Autowired
	private CollaborateDao collaborateDao;
	@Autowired
	private TextViewSettingDao textViewSettingDao;
	@Autowired
	private FolderController folderController;
	@Autowired
	private AlreadyReadDao alreadyReadDao;

	@Override
	public GetFolderResponse getFolder(GetFolderRequest getFolderRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(getFolderRequest, true,
				true);

		BaseMetadata metadata = metadataFactory.getBaseMetadataByPath(pathInfo);
		if (metadata == null || !metadata.isFolder()) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		List<? extends BaseMetadata> childMetadatas = metadataFactory
				.getBaseMetadatasByParentId(metadata.getId(),
						pathInfo.isUnderMyBackupData(), false);

		String userId = getFolderRequest.getUserId();
		String ownerId = pathInfo.getOwnerId();
		String parentPath = metadataFactory.getMetadataRelativePath(metadata,
				getFolderRequest.getUserId());
		List<FolderResponse> folderResponses = metadataFactory
				.toFolderResponses(childMetadatas,
						getFolderRequest.isDisplayNote(), parentPath);
		List<FileResponse> fileResponses = metadataFactory.toFileResponses(
				getFolderRequest.getToken(), childMetadatas,
				getFolderRequest.isDisplayNote(), parentPath);
		boolean hasImage = checkHasImage(childMetadatas);
		PageProfileProperty pageProfileProperty = metadataFactory
				.computePageProfileProperty(pathInfo, metadata.getId(),
						getFolderRequest.getViewerDeviceUniqueId(),
						getFolderRequest.getForceSortByType());
		int totalFolderSize = folderResponses.size();
		metadataFactory.sortFolderResponses(folderResponses,
				pageProfileProperty.getSortByType());
		metadataFactory.sortFileResponses(fileResponses,
				pageProfileProperty.getSortByType());
		folderResponses = metadataFactory.pagingFolderResponses(
				folderResponses, getFolderRequest.getPageNumber(),
				getFolderRequest.getItemsPerPage(), 0);
		fileResponses = metadataFactory.pagingFileResponses(fileResponses,
				getFolderRequest.getPageNumber(),
				getFolderRequest.getItemsPerPage(), totalFolderSize);
		metadataFactory.updateFolderResponsesAfterPaging(folderResponses,
				userId, ownerId);
		metadataFactory.updateFileResponsesAfterPaging(fileResponses, userId,
				metadata, ownerId);
		ItemStyle itemStyle = null;
		if (metadata instanceof Metadata) {
			metadataFactory.adjustItemStyleByGlobalItemStyle((Metadata)metadata, ownerId);
			itemStyle = ItemStyle.fromMetadata((Metadata)metadata);
		}
		// result
		int totalPages = MathUtils.ceil(childMetadatas.size(),
				getFolderRequest.getItemsPerPage(), 1);
		return new GetFolderResponse(totalPages, pageProfileProperty, hasImage,
				itemStyle, folderResponses, fileResponses);
	}
	
	@Override
	public FolderResponse getFolderDetail(PathRequestBase pathRequestBase)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(pathRequestBase, true, true);
		BaseMetadata folder = metadataFactory.getBaseMetadataByPath(pathInfo);
		
		if (folder == null || !folder.isFolder()) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}
		
		String folderPath = metadataFactory.getMetadataRelativePath(folder, pathRequestBase.getUserId());
		String parentPath = FilenameUtils.getPathNoEndSeparator(folderPath);
		List<FolderResponse> folderResponses = metadataFactory.toFolderResponses(Arrays.asList(folder), true, parentPath);
		
		metadataFactory.updateFolderResponsesAfterPaging(folderResponses, pathRequestBase.getUserId(), pathInfo.getOwnerId());
		
		return folderResponses.get(0);
	}

	private boolean checkHasImage(List<? extends BaseMetadata> childMetadatas) {
		for (BaseMetadata singleMetadata : childMetadatas) {
			if (!singleMetadata.isFolder()
					&& singleMetadata.isPicture(pathFactory)) {
				return true;
			}
		}
		return false;
	}

	@Override
	@Transactional
	public Boolean createFolder(PathRequestBase createFolderRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(createFolderRequest,
				true, false);

		Metadata parentFolder = metadataDao.getMetadataByPath(FilenameUtils
				.getFullPathNoEndSeparator(pathInfo.getFullOwnerPath()),
				pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		if (parentFolder == null || !parentFolder.isFolder()) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}
		Metadata folder = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (folder != null) {
			throw new MetadataException(HttpStatus.FOLDER_EXIST);
		}
		// get max sort priority
		int sortPriority = metadataDao.getMaxSortPriority(parentFolder.getId());
		folder = metadataFactory.buildBase4Create(pathInfo);
		folder.setParentId(parentFolder.getId());
		folder.setSortPriority(sortPriority + 1);
		folder.setSharedRootId(parentFolder.getSharedRootId());
		folder.setSyncRootId(parentFolder.getSyncRootId());
		folder.setFolder(true);
		if (createFolderRequest instanceof UpdateFolderRequest) {
			folder.setNote(((UpdateFolderRequest) createFolderRequest)
					.getNote());
			folder.setIconBlockId(((UpdateFolderRequest) createFolderRequest)
					.getIconBlockId());
			folder.setIconText(((UpdateFolderRequest) createFolderRequest)
					.getIconText());
			folder.setIconTextColor(((UpdateFolderRequest) createFolderRequest)
					.getIconTextColor());
			folder.setIconTextStyle(((UpdateFolderRequest) createFolderRequest)
					.getIconTextStyle());
		}
		metadataDao.createMetadata(folder);
		backupMetadataService.createSingleBackup(folder, pathInfo.getOwnerId());
		// create self attr and parent
		MetadataAttr attr = new MetadataAttr();
		attr.setMetadataIndexId(folder.getId());
		metadataAttrService.createMetadataAttr(attr);
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.addEmptyFolder(folder.getParentId());
		countService.updateAllParentCount(countManageModel);

		// sync
		if (!syncRelationService.isCaller()) {
			syncRelationService.sync(createFolderRequest, folder,
					SystemEvent.CREATE_FOLDER);
		}
		xmppService.sendToAllDevices(SystemEvent.UPDATE_FOLDER, pathInfo);
		// activity history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(folder
				.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", folder.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.CREATE_FOLDER.toString(),
				createFolderRequest.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
		return true;
	}

	@Override
	public Boolean updateFolder(PathRequestBase upadateFolderRequest)
			throws MetadataException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	@Transactional
	public void deleteFolder(String folderId, String userId, boolean extract,
			boolean isUnderMyBackupData) throws MetadataException {

		// check : if under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao
				.getFileProcessingStatus(folderId);
		if (fileProcessingStatus != null
				&& fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}

		BaseMetadata folder;
		List<? extends BaseMetadata> list;
		if (isUnderMyBackupData) {
			BackupMetadata backupMetadata = backupMetadataDao
					.getBackup(folderId);
			if (!StringUtils.isNullOrEmpty(backupMetadata.getOriginalIndexId())) {
				throw new MetadataException(HttpStatus.CANT_DELETE_THIS_FOLDER);
			}
			List<BackupMetadata> backupMetadataList = backupMetadataService
					.getAllSubFoldersAndFiles(backupMetadata);
			if (!extract) {
				backupMetadataList.add(backupMetadata);
			}
			folder = backupMetadata;
			list = backupMetadataList;
		} else {
			Metadata metadata = metadataDao.getMetadata(folderId);
			if (metadata == null) {
				throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
			}
			List<Metadata> metadataList = metadataFactory
					.getAllSubFoldersAndFiles(metadata);
			if (!extract) {
				metadataList.add(metadata);
			}
			folder = metadata;
			list = metadataList;

			// delete sync relations
			for (String syncRootId : syncRelationService.getAllSubSyncFolderId(
					folder, !extract, list)) {
				syncRelationService.deleteRelatedSyncRelation(syncRootId,
						userId);
			}
		}

		for (BaseMetadata metadata : list) {
			if (isUnderMyBackupData) {
				backupMetadataDao.deleteBackupById(metadata.getId());
				metadataAttrService.deleteMetadataAttr(metadata.getId());
			} else {
				metadataDao.deleteMetadata(metadata.getId());
				backupMetadataService.unbackupMetadata(metadata.getId());
				metadataAttrService.deleteMetadataAttr(metadata.getId());
				if (metadata.getSharedRootId().equals(metadata.getId())) {
					// shared folder(owner): delete collaborate & all ref_link
					collaborateDao.deleteCollaborate(metadata.getId());
					deleteRelativeRefLinks(metadata.getId());
				} else if (metadata.getType().equals(
						MetadataType.REFLINK.toString())) {
					// ref_link(member): delete owner's folder (include
					// collaborate & all ref_link)
					deleteFolder(metadata.getRefId(), userId, false, false);
				}
			}
		}

		String mainMetadataId = "";
		int folderCount = 0;
		if (!extract) {
			mainMetadataId = folder.getParentId();
			folderCount = folder.getFoldersCount() + 1;
		} else {
			mainMetadataId = folder.getId();
			folderCount = folder.getFoldersCount();
		}
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.deleteFolder(mainMetadataId, folder.getTotalSize(),
				folder.getFilesCount(), folderCount,
				folder.getCollaborateCount(), folder.getSyncRootCount(),
				folder.getRefLinkCount());
		if (isUnderMyBackupData) {
			countService.updateAllParentCountBackup(countManageModel);
		} else {
			countService.updateAllParentCount(countManageModel);
		}
	}

	private void deleteRelativeRefLinks(String refId) {
		List<Metadata> refLinks = metadataDao.getMetadataByRefIdOnly(refId);
		for (Metadata refLink : refLinks) {
			metadataDao.deleteMetadata(refLink.getId());
			CountManageModel countManageModel = new CountManageModel();
			countManageModel.deleteFolder(refLink.getParentId(), 0, 0, 1, 0, 0,
					1);
			countService.updateAllParentCount(countManageModel);
		}
	}

	@Override
	@Transactional
	public void deleteFolder(DeleteFolderRequest deleteFolderRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(deleteFolderRequest,
				true, true);
		deleteFolderForSuccessValidate(deleteFolderRequest, pathInfo);
	}

	// for member delete owner folder by collaborate/delete
	@Override
	@Transactional
	public void deleteFolderForSuccessValidate(
			DeleteFolderRequest deleteFolderRequest, PathInfo pathInfo) {

		BaseMetadata metadata = metadataFactory.getFolderByPathInfo(pathInfo);

		deleteFolder(metadata.getId(), deleteFolderRequest.getUserId(),
				deleteFolderRequest.isExtract(), pathInfo.isUnderMyBackupData());

		// send dynamoDB history for target file
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata
				.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.DELETE_FOLDER.toString(),
				metadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.DELETE_FOLDER, pathInfo);

		// sync
		if (!syncRelationService.inCallerList()) {
			syncRelationService.sync(deleteFolderRequest, metadata,
					SystemEvent.DELETE_FOLDER);
		}

	}

	@Override
	@Transactional
	public void renameFolder(RenameFolderRequest renameFolderRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(renameFolderRequest,
				true, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		// check : if under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao
				.getFileProcessingStatus(metadata.getId());
		if (fileProcessingStatus != null
				&& fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}

		String ownerFolderPath = FilenameUtils.getFullPath(pathInfo
				.getFullOwnerPath());
		StringBuilder fullPath = new StringBuilder();
		String newFullPath = fullPath.append(ownerFolderPath)
				.append(renameFolderRequest.getNewName()).toString();
		Metadata folderNameSearchResult = metadataDao.getMetadataByPath(
				newFullPath, pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());

		if (folderNameSearchResult != null) {
			throw new MetadataException(HttpStatus.FOLDER_EXIST);
		} else if (metadata.getType().equals(MetadataType.COMMON.toString())
				|| metadata.getType().equals(
						MetadataType.SHARECOMMON.toString())) {
			throw new MetadataException(HttpStatus.FOLDER_TYPE_IS_COMMON);
		} else if (StringUtils.isNullOrEmpty(metadata.getName())) {
			throw new MetadataException(HttpStatus.NAME_IS_EMPTY);
		} else {
			metadataDao.updateNewName(metadata.getId(),
					renameFolderRequest.getNewName(), "");
			metadata.setName(renameFolderRequest.getNewName().toLowerCase());
			metadata.setOriginName(renameFolderRequest.getNewName());
			backupMetadataService.renameBackup(metadata);
			// update all reflink
			if (metadata.getSharedRootId().equals(metadata.getId())) {
				if (checkNameExistInMemberFolder(metadata.getSharedRootId(),
						renameFolderRequest.getNewName())) {
					throw new MetadataException(HttpStatus.FOLDER_EXIST);
				}
				metadataDao.updateNewNameToReflink(metadata.getId(),
						renameFolderRequest.getNewName(), "");
			}
			// send dynamoDB history
			String sharedUpperPath = metadataFactory
					.getSharedUpperPath(metadata.getSharedRootId());
			dynamoDBService.createActivityHistory(newFullPath,
					pathInfo.getFullOwnerPath(), sharedUpperPath,
					sharedUpperPath, metadata.getId(), pathInfo.getUserId(),
					DateUtils.nowUTCDateTime(),
					ChangeReason.RENAME_FOLDER.toString(),
					metadata.getSharedRootId(),
					ActivityHistoryLevel.NORMAL.toString());
			// send XMPP
			PathInfo newPathInfo = pathInfo.clone();
			newPathInfo.setFullOwnerPath(newFullPath);
			xmppService.sendToAllDevices(SystemEvent.RENAME_FOLDER, pathInfo,
					newPathInfo);
			// sync
			if (!syncRelationService.isCaller()) {
				syncRelationService.sync(renameFolderRequest, metadata,
						SystemEvent.RENAME_FOLDER);
			}
		}
	}

	private boolean checkNameExistInMemberFolder(String refId, String name) {
		List<Metadata> refLinks = metadataDao.getMetadataByRefIdOnly(refId);
		if (refLinks == null) {
			return false;
		}
		for (Metadata refLink : refLinks) {
			Metadata metadata = metadataDao.getMetadataByCriteria(
					refLink.getParentId(), name, "", "");
			if (metadata != null) {
				return true;
			}
		}
		return false;
	}

	@Override
	@Transactional
	public void moveFolder(MoveFolderRequest request) throws MetadataException {
		// List<String> sourceShareFolderId = new ArrayList<String>();
		// List<String> targetShareFolderId = new ArrayList<String>();
		// StringUtils.writeJSON(request);
		String userId = request.getUserId();
		String rootId = metadataDao.getRootMetadataId(userId);
		boolean extract = request.isExtract();
		if (StringUtils.isNullOrEmpty(rootId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		PathInfo pathInfo = pathFactory.parsePathInfo(request, true, true);

		String destOwnerId = StringUtils.isNullOrEmpty(request
				.getDestinationOwnerId()) ? userId : request
				.getDestinationOwnerId();
		String targetFolderPath = "".equals(request.getDestination()) ? destOwnerId
				: String.format("%s/%s", destOwnerId, request.getDestination());
		String targetOrignalFolderPath = String.format("%s/%s",
				targetFolderPath, FilenameUtils.getBaseName(request.getPath()));
		String safeFullOwnerPath = pathInfo.getFullOwnerPath().toLowerCase();
		String[] pathFragmenets = safeFullOwnerPath.split("/");

		int pathLen = pathFragmenets.length;

		// BaseMetadata sourceFolderDir = null;
		BaseMetadata sourceFolder = null;

		List<? extends BaseMetadata> tempMetadata = new ArrayList<BaseMetadata>(
				2);

		boolean fromBackup = pathInfo.isUnderMyBackupData();
		if (fromBackup) {
			tempMetadata = backupMetadataDao.getBackupMetadatasByPath(
					(String[]) ArrayUtils.remove(pathFragmenets, 1),
					pathInfo.getDeviceUid(), pathInfo.getFullSourcePath(),
					pathLen - 3, true);
		} else {
			tempMetadata = metadataDao.getMetadatasByPath(pathFragmenets, "",
					"", pathLen - 2);
		}

		// sourceFolderDir = tempMetadata.get(0);
		sourceFolder = tempMetadata.get(1);

		// validation
		if (sourceFolder == null)
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);

		if (!sourceFolder.isFolder()) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		// check : if under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao
				.getFileProcessingStatus(sourceFolder.getId());
		if (fileProcessingStatus != null
				&& fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}

		Metadata targetFolderDir = (Metadata) metadataFactory.getFolder(userId,
				request.getDestination(), destOwnerId,
				request.getDestinationSharedRootId());

		if (targetFolderDir == null)
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);

		if (!targetFolderDir.isFolder()) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}
		if (!StringUtils.isNullOrEmpty(targetFolderDir.getSyncRootId())) {
			if (StringUtils.isNullOrEmpty(sourceFolder.getSyncRootId())) {
				throw new MetadataException(HttpStatus.CANT_MOVE_TO_SYNC_FOLDER);
			} else if (!targetFolderDir.getSyncRootId().toLowerCase()
					.equals(sourceFolder.getSyncRootId().toLowerCase())) {
				throw new MetadataException(
						HttpStatus.CANT_MOVE_TO_OTHER_SYNC_FOLDER);
			}
		}

		if ((targetFolderPath.toLowerCase().concat("/"))
				.indexOf(safeFullOwnerPath.concat("/")) == 0) {
			if (!request.isExtract()
					|| !safeFullOwnerPath.equals(targetFolderPath)) {
				throw new MetadataException(HttpStatus.CANT_MOVE_FOLDER_FROM);
			}
		}

		pathFactory.checkCanCopyFolderFrom(request.getPath());
		pathFactory.checkCanMoveFolderTo(request.getDestination());

		Queue<BaseMetadata> copyMetadatas = new ArrayDeque<BaseMetadata>(1024);
		if (request.isExtract()) {
			copyMetadatas
					.addAll(getSourceSubMetadata(sourceFolder, fromBackup));
			for (BaseMetadata submetadata : copyMetadatas) {
				pathFactory.checkCanCopyFolderFrom(request.getPath()
						.concat("/").concat(submetadata.getName()));
				if (request.isOverride()) {
					pathFactory
							.checkCanCopyFolderToBeOverride(targetOrignalFolderPath
									.concat("/").concat(submetadata.getName()));
				}
			}
		} else {
			copyMetadatas.add(sourceFolder);
		}

		if (request.isOverride()) {
			pathFactory.checkCanCopyFolderToBeOverride(targetOrignalFolderPath);
		}

		//
		// starting
		//

		if (sourceFolder.getSharedRootId().equals(sourceFolder.getId())
				&& !request.getUserId().equals(sourceFolder.getOwnerId())
				&& MetadataType.SHARECOMMON.toString().equalsIgnoreCase(
						targetFolderDir.getType()) == false) {
			memberMoveShareFolder(sourceFolder, targetFolderDir, userId,
					request.getDestination());
			otherLogic(targetFolderDir, targetFolderPath, destOwnerId,
					request.getDestinationSharedRootId(), pathInfo, request,
					sourceFolder.getSharedRootId(),
					ChangeReason.MOVE_FOLDER.getChangeReason(),
					SystemEvent.MOVE_FOLDER);
			return;
		}

		// transform to shared folder for normal folder only (under my shared
		// folders/)
		if (fromBackup == false
				&& MetadataType.SHARECOMMON.toString().equalsIgnoreCase(
						targetFolderDir.getType())) {
			for (BaseMetadata copyMetadata : copyMetadatas) {
				if (copyMetadata.isFolder() == false
						|| copyMetadata.hasCollaborate()
						|| copyMetadata.hasRefLink()
						|| copyMetadata.hasSyncFolder()
						|| copyMetadata.getType().equals(
								MetadataType.NORMAL.toString()) == false) {
					throw new MetadataException(HttpStatus.CANT_MOVE_FOLDER_TO);
				}
				String sourceOriginalPath = metadataFactory
						.getMetadataOriginalPath(copyMetadata);
				collaborateService
						.transformToShared(new CreateCollaborateRequest(request,
								sourceOriginalPath, "", copyMetadata.getId(), userId));
				copyMetadata.setType(MetadataType.SHARE.toString());
				copyMetadata.setSharedRootId(copyMetadata.getId());
			}
		}

		FolderMapItem tempMetaMapping = null;
		String targetSharedRootId = targetFolderDir.getSharedRootId();
		String targetSyncRootId = targetFolderDir.getSyncRootId();
		Metadata tempTarget = null, copyedMeta = null;
		List<String> sourceIds = new ArrayList<String>();
		List<? extends BaseMetadata> tempSourceMetadatas = null;

		String prevPId = sourceFolder.getParentId();

		String targetCurPid = targetFolderDir.getId();
		List<Metadata> tempTargetMetadatas = findChildren(targetCurPid);
		// false --> contain true ----> sub
		if (request.isExtract()) {
			prevPId = sourceFolder.getId();
		} else {
			if (!request.isOverride()
					&& (null != tempTargetMetadatas && null != (tempTarget = getSameName(
							tempTargetMetadatas, sourceFolder)))) {
				targetFolderPath = String.format("%s/%s", targetFolderPath,
						metadataFactory.generateName(tempTargetMetadatas,
								tempTarget, true, true));
			} else {
				targetFolderPath = targetOrignalFolderPath;
			}
		}

		Map<String, FolderMapItem> foldersMaps = new HashMap<String, FolderMapItem>(
				100);
		foldersMaps.put(prevPId, new FolderMapItem(targetFolderDir.getId(),
				true));
		MetadataAttr metadataAttr = metadataAttrDao
				.getMetadataAttr(sourceFolder.getId());
		BaseMetadata tempMeta = null;
		List<Metadata> createMetas = new ArrayList<Metadata>(100);
		List<SharedRootMap> updateShareRoots = new ArrayList<SharedRootMap>(100);
		List<MetadataAttr> updateAttrs = new LinkedList<MetadataAttr>();

		Set<String> alreadyReadSet = new HashSet<String>();

		List<String> deleteMetadas = new ArrayList<String>(50);
		List<String> deleteBackups = new ArrayList<String>(50);
		List<SourceTargetMap> filesMaps = new ArrayList<SourceTargetMap>(100);
		List<CountAffected> affecteds = new ArrayList<CountAffected>(50);
		CountAffected tempAffected = new CountAffected(targetCurPid);
		CountAffected srcAffected = new CountAffected();
		srcAffected.setParentId(extract == true ? sourceFolder.getId()
				: sourceFolder.getParentId());
		srcAffected.addFileCount(-metadataAttr.getFilesCount());
		srcAffected.addFolderCount(-metadataAttr.getFoldersCount());
		srcAffected.addSize(-metadataAttr.getTotalSize());
		srcAffected.addCollaborateCount(-metadataAttr.getCollaborateCount());
		affecteds.add(srcAffected);
		affecteds.add(tempAffected);
		boolean hasShareFolder = false;
		while ((tempMeta = copyMetadatas.poll()) != null) {
			if (!StringUtils.isNullOrEmpty(targetFolderDir.getSharedRootId())) {
				if (tempMeta.getType().equals(MetadataType.SHARE.toString())) {
					throw new MetadataException(
							HttpStatus.CANT_MOVE_FOLDER_FROM);
				}
				if (sourceFolder.getType().equals(
						MetadataType.NORMAL.toString())
						&& tempMeta.getType().equals(
								MetadataType.REFLINK.toString())) {
					throw new MetadataException(
							HttpStatus.CANT_MOVE_FOLDER_FROM);
				}
				tempMeta.setSharedRootId(targetFolderDir.getSharedRootId());
			} else {
				if (tempMeta.getType().equals(MetadataType.SHARE.toString())) {
					hasShareFolder = true;
					if (!tempMeta.getOwnerId().equals(
							targetFolderDir.getOwnerId())) {
						throw new MetadataException(
								HttpStatus.CANT_MOVE_FOLDER_FROM);
					}
				}
				if (!hasShareFolder) {
					tempMeta.setSharedRootId(targetFolderDir.getSharedRootId());
				}
			}

			tempMeta.setOwnerId(targetFolderDir.getOwnerId());
			sourceIds.add(tempMeta.getId());
			if (!prevPId.equals(tempMeta.getParentId())) {
				prevPId = tempMeta.getParentId();
				tempMetaMapping = foldersMaps.get(tempMeta.getParentId());
				targetCurPid = tempMetaMapping.getCurrentPId();
				tempAffected = new CountAffected(targetCurPid);
				affecteds.add(tempAffected);
				if (null != tempTargetMetadatas
						&& tempMetaMapping.isDuplicate()) {
					tempTargetMetadatas = findChildren(tempMetaMapping
							.getTargetId());
				} else {
					tempTargetMetadatas = null;
				}
			}

			if (tempMeta.isFolder()) {
				tempSourceMetadatas = getSourceSubMetadata(tempMeta, fromBackup);
				copyMetadatas.addAll(tempSourceMetadatas);
			}

			if (!StringUtils.isNullOrEmpty(tempTargetMetadatas)) {
				tempTarget = getSameName(tempTargetMetadatas, tempMeta);

				if (null != tempTarget) {
					if (request.isOverride()) {
						if (tempMeta.isFolder() == tempTarget.isFolder()) {
							// file --> file , folder -> folder merge

							copyedMeta = copyMetadata(tempMeta, tempTarget,
									userId, destOwnerId, targetSharedRootId,
									targetSyncRootId, targetCurPid, true);
							if (!tempMeta.isFolder()) {
								// file --> file , affect -->size
								// affectSize += (tempMeta.getTotalSize() -
								// tempTarget.getTotalSize());
								// model.updateFile(tempMeta.getParentId(),
								// affectSize);
								filesMaps.add(new SourceTargetMap(tempMeta
										.getId(), tempTarget.getId()));

								// update width & height
								if (copyedMeta.getWidth() != tempTarget
										.getWidth()
										|| copyedMeta.getHeight() != tempTarget
												.getHeight()) {
									updateAttrs
											.add(new MetadataAttr(copyedMeta
													.getId(), false, copyedMeta
													.getWidth(), copyedMeta
													.getHeight()));
								}

							} else {
								// folder --> folder, affect --> nothing,
								// but
								// folder may be affect so add to affects
								foldersMaps.put(tempMeta.getId(),
										new FolderMapItem(tempTarget.getId(),
												copyedMeta.getId(), true));
								tempTargetMetadatas.remove(tempTarget);
								addModifySharedRoot(updateShareRoots,
										tempTargetMetadatas);
								if (copyedMeta.getType().equals(
										MetadataType.SHARE.toString())) {
								}
							}

							createMetas.add(copyedMeta);
							updateAlreadyReadSet(alreadyReadSet,
									request.getUserId(), tempMeta, copyedMeta);
							deleteMetadas.add(tempTarget.getId());
							deleteBackups.add(tempTarget.getId());

						} else if (request.isForceOverride()) {
							// force delete & create
							tempTarget.setFolder(tempMeta.isFolder());

							// affectSize += (tempMeta.getTotalSize() -
							// tempTarget.getTotalSize());
							copyedMeta = copyMetadata(tempMeta, tempTarget,
									userId, destOwnerId, targetSharedRootId,
									targetSyncRootId, targetCurPid, true);
							if (tempMeta.isFolder()) {
								// target is a file, affect --> folder: +1,
								// file: -1 change to folder
								foldersMaps.put(tempMeta.getId(),
										new FolderMapItem(tempTarget.getId(),
												copyedMeta.getId()));

							} else {
								filesMaps.add(new SourceTargetMap(tempMeta
										.getId(), tempTarget.getId()));
							}

							// tempTarget is folder just delete self to
							deleteMetadas.add(tempTarget.getId());
							createMetas.add(copyedMeta);
							updateAlreadyReadSet(alreadyReadSet,
									request.getUserId(), tempMeta, copyedMeta);
							deleteBackups.add(tempTarget.getId());

						} else {
							throw new MetadataException(HttpStatus.FILE_EXIST);
						}
						// delete backup if exist by id
						if (copyedMeta.isEncrypted()) {
							deleteBackups.add(tempTarget.getId());
						}
					} else { // override = false

						if (tempMeta.isFolder() != tempTarget.isFolder()) {
							throw new MetadataException(HttpStatus.FILE_EXIST);
						}

						String availableName = metadataFactory.generateName(
								tempTargetMetadatas, tempTarget,
								tempTarget.isFolder(), true);

						copyedMeta = copyMetadata(tempMeta, null, userId,
								destOwnerId, targetSharedRootId,
								targetSyncRootId, targetCurPid, true);
						copyedMeta.setName(availableName.toLowerCase());
						copyedMeta.setOriginName(availableName);
						createMetas.add(copyedMeta);
						updateAlreadyReadSet(alreadyReadSet,
								request.getUserId(), tempMeta, copyedMeta);

						if (tempMeta.isFolder()) {
							foldersMaps.put(tempMeta.getId(),
									new FolderMapItem(copyedMeta.getId()));
						} else {
							filesMaps.add(new SourceTargetMap(tempMeta.getId(),
									copyedMeta.getId()));
						}
					}
					// if(copyedMeta.getType().equals(MetadataType.SHARE.toString())){
					// CountAffected affected = new CountAffected();
					// affected.setParentId(copyedMeta.getId());
					// affected.setCollaborateCount(1);
					// affecteds.add(affected);
					// }
					continue;
				}// !!
			}

			// no same name, create
			copyedMeta = copyMetadata(tempMeta, null, userId, destOwnerId,
					targetSharedRootId, targetSyncRootId, targetCurPid, true);

			createMetas.add(copyedMeta);
			updateAlreadyReadSet(alreadyReadSet, request.getUserId(), tempMeta,
					copyedMeta);

			if (tempMeta.isFolder()) {
				// addAffect(folderAffects, tempTarget, 0);
				foldersMaps.put(tempMeta.getId(),
						new FolderMapItem(copyedMeta.getId(), false));

				tempAffected.addFolderCount(1);

			} else {
				// affectSize += copyedMeta.getTotalSize();
				filesMaps.add(new SourceTargetMap(tempMeta.getId(), copyedMeta
						.getId()));
				tempAffected.addFileCount(1);
				tempAffected.addSize(copyedMeta.getTotalSize());
			}
		}
		for (Metadata item : createMetas) {
			if (item.getType().equals(MetadataType.SHARE.toString())) {
				CountAffected affected = new CountAffected();
				affected.setParentId(item.getId());
				affected.setCollaborateCount(1);
				affecteds.add(affected);
			}
		}
		deleteMetadas.addAll(sourceIds);
		updateAll2Db(userId, fromBackup, affecteds, foldersMaps,
				createMetas, updateAttrs, deleteMetadas, deleteBackups,
				filesMaps, alreadyReadSet);
		otherLogic(targetFolderDir, targetFolderPath, destOwnerId,
				request.getDestinationSharedRootId(), pathInfo, request,
				sourceFolder.getSharedRootId(),
				ChangeReason.MOVE_FOLDER.getChangeReason(),
				SystemEvent.MOVE_FOLDER);
	}

	private void memberMoveShareFolder(BaseMetadata source,
			Metadata targetFolderDir, String userId, String destination) {
		long nowUTCTime = DateUtils.nowUTCTimestamp();
		if (!"".equals(targetFolderDir.getSharedRootId())
				|| !"".equals(targetFolderDir.getSyncRootId())) {
			throw new MetadataException(HttpStatus.CANT_MOVE_FOLDER_FROM);
		}
		String destinationPath = "".equals(destination) ? String.format(
				"%s/%s", userId, source.getName()) : String.format("%s/%s/%s",
				userId, destination, source.getName());
		Metadata folder = metadataDao
				.getMetadataByPath(destinationPath, "", "");
		if (folder != null) {
			throw new MetadataException(HttpStatus.FOLDER_EXIST);
		}
		Metadata refMetadataId = metadataDao.getMetadataByRefId(userId,
				source.getId());
		if (refMetadataId != null) {
			metadataDao.deleteMetadata(refMetadataId.getId());
			CountManageModel countManageModel = new CountManageModel();
			countManageModel.deleteFolder(refMetadataId.getId(), 0, 0, 1, 0, 0,
					1);
			countService.updateAllParentCount(countManageModel);
		}
		Metadata metadataObj = new Metadata();
		metadataObj.setId(StringUtils.getUUID());
		metadataObj.setParentId(targetFolderDir.getId());
		metadataObj.setName(source.getName());
		metadataObj.setFolder(true);
		metadataObj.setOriginName(source.getOriginName());
		metadataObj.setModifiedAt(nowUTCTime);
		metadataObj.setModifiedBy(userId);
		metadataObj.setBackupCount(-1);
		metadataObj.setNote("");
		metadataObj.setEncrypted(false);
		metadataObj.setCreatedAt(nowUTCTime);
		metadataObj.setCreatedBy(userId);
		metadataObj.setType(MetadataType.REFLINK.toString());
		metadataObj.setOwnerId(userId);
		metadataObj.setRefId(source.getId());
		metadataObj.setRefToOwnerId(source.getOwnerId());
		int sortPriority = metadataDao.getMaxSortPriority(targetFolderDir
				.getId());
		metadataObj.setSortPriority(sortPriority);
		metadataDao.createMetadata(metadataObj);
		// create self attr and parent
		MetadataAttr attr = new MetadataAttr();
		attr.setMetadataIndexId(metadataObj.getId());
		attr.setRefLinkCount(1);
		metadataAttrService.createMetadataAttr(attr);
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.addFolder(metadataObj.getParentId(), 0, 0, 1, 0, 0, 1);
		countService.updateAllParentCount(countManageModel);
	}

	@Override
	@Transactional
	public void copyFolder(CopyFolderRequest request) throws MetadataException {

		String userId = request.getUserId();
		String rootId = metadataDao.getRootMetadataId(userId);
		if (StringUtils.isNullOrEmpty(rootId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		PathInfo pathInfo = pathFactory.parsePathInfo(request, true, true);

		String destOwnerId = StringUtils.isNullOrEmpty(request
				.getDestinationOwnerId()) ? userId : request
				.getDestinationOwnerId();
		String targetFolderPath = "".equals(request.getDestination()) ? destOwnerId
				: String.format("%s/%s", destOwnerId, request.getDestination());
		String targetOrignalFolderPath = String.format("%s/%s",
				targetFolderPath, FilenameUtils.getBaseName(request.getPath()));

		String safeFullOwnerPath = pathInfo.getFullOwnerPath().toLowerCase();
		String[] pathFragmenets = safeFullOwnerPath.split("/");

		int pathLen = pathFragmenets.length;

		// BaseMetadata sourceFolderDir = null;
		BaseMetadata sourceFolder = null;

		List<? extends BaseMetadata> tempMetadata = new ArrayList<BaseMetadata>(
				2);

		boolean fromBackup = pathInfo.isUnderMyBackupData();
		if (fromBackup) {
			tempMetadata = backupMetadataDao.getBackupMetadatasByPath(
					(String[]) ArrayUtils.remove(pathFragmenets, 1),
					pathInfo.getDeviceUid(), pathInfo.getFullSourcePath(),
					pathLen - 3, true);
		} else {
			tempMetadata = metadataDao.getMetadatasByPath(pathFragmenets, "",
					"", pathLen - 2);
		}

		// sourceFolderDir = tempMetadata.get(0);
		sourceFolder = tempMetadata.get(1);

		// validation
		if (sourceFolder == null)
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);

		if (!sourceFolder.isFolder()) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}
		// check : if under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao
				.getFileProcessingStatus(sourceFolder.getId());
		if (fileProcessingStatus != null
				&& fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}

		Metadata targetFolderDir = (Metadata) metadataFactory.getFolder(userId,
				request.getDestination(), destOwnerId,
				request.getDestinationSharedRootId());

		if (targetFolderDir == null)
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);

		if (!targetFolderDir.isFolder()) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		if (!StringUtils.isNullOrEmpty(targetFolderDir.getSyncRootId())) {
			if (StringUtils.isNullOrEmpty(sourceFolder.getSyncRootId())) {
				throw new MetadataException(HttpStatus.CANT_COPY_TO_SYNC_FOLDER);
			} else if (!targetFolderDir.getSyncRootId().toLowerCase()
					.equals(sourceFolder.getSyncRootId().toLowerCase())) {
				throw new MetadataException(
						HttpStatus.CANT_COPY_TO_OTHER_SYNC_FOLDER);
			}
		}

		if ((targetFolderPath.toLowerCase().concat("/"))
				.indexOf(safeFullOwnerPath.concat("/")) == 0) {
			if (!request.isExtract()
					|| !safeFullOwnerPath.equals(targetFolderPath)) {
				throw new MetadataException(HttpStatus.CANT_COPY_FOLDER_FROM);
			}
		}

		pathFactory.checkCanCopyFolderFrom(request.getPath());
		pathFactory.checkCanCopyFolderTo(request.getDestination());

		Queue<BaseMetadata> copyMetadatas = new ArrayDeque<BaseMetadata>(1024);
		if (request.isExtract()) {
			copyMetadatas
					.addAll(getSourceSubMetadata(sourceFolder, fromBackup));
			for (BaseMetadata submetadata : copyMetadatas) {
				pathFactory.checkCanCopyFolderFrom(request.getPath()
						.concat("/").concat(submetadata.getName()));
				if (request.isOverride()) {
					pathFactory
							.checkCanCopyFolderToBeOverride(targetOrignalFolderPath
									.concat("/").concat(submetadata.getName()));
				}
			}
		} else {
			copyMetadatas.add(sourceFolder);
		}

		if (request.isOverride()) {
			pathFactory.checkCanCopyFolderToBeOverride(targetOrignalFolderPath);
		}

		FolderMapItem tempMetaMapping = null;
		String targetSharedRootId = targetFolderDir.getSharedRootId();
		String targetSyncRootId = targetFolderDir.getSyncRootId();
		Metadata tempTarget = null, copyedMeta = null;

		List<? extends BaseMetadata> tempSourceMetadatas = null;
		// long affectSize = 0L;

		// false --> contain true ----> sub

		String prevPId = sourceFolder.getParentId();
		String targetCurPid = targetFolderDir.getId();
		List<Metadata> tempTargetMetadatas = findChildren(targetCurPid);
		// false --> contain true ----> sub
		if (request.isExtract()) {
			prevPId = sourceFolder.getId();
		} else {
			if (!request.isOverride()
					&& (null != tempTargetMetadatas && null != (tempTarget = getSameName(
							tempTargetMetadatas, sourceFolder)))) {
				targetFolderPath = String.format("%s/%s", targetFolderPath,
						metadataFactory.generateName(tempTargetMetadatas,
								tempTarget, true, false));
			} else {
				targetFolderPath = targetOrignalFolderPath;
			}
		}

		Map<String, FolderMapItem> foldersMaps = new HashMap<String, FolderMapItem>(
				100);
		foldersMaps.put(prevPId, new FolderMapItem(targetCurPid, true));

		BaseMetadata tempMeta = null;
		List<Metadata> createMetas = new ArrayList<Metadata>(100);
		List<MetadataAttr> updateAttrs = new LinkedList<MetadataAttr>();

		List<String> deleteMetadas = new ArrayList<String>(50);
		List<String> deleteBackups = new ArrayList<String>(50);
		List<SourceTargetMap> filesMaps = new ArrayList<SourceTargetMap>(100);

		Set<String> alreadyReadSet = new HashSet<String>();

		List<CountAffected> affecteds = new ArrayList<CountAffected>(50);
		CountAffected tempAffected = new CountAffected(targetCurPid);
		affecteds.add(tempAffected);
		int sortPriority = metadataDao.getMaxSortPriority(prevPId) + 1;

		while ((tempMeta = copyMetadatas.poll()) != null) {
			if (!prevPId.equals(tempMeta.getParentId())) {
				prevPId = tempMeta.getParentId();
				tempMetaMapping = foldersMaps.get(tempMeta.getParentId());
				targetCurPid = tempMetaMapping.getTargetId();
				tempAffected = new CountAffected(targetCurPid);
				sortPriority = metadataDao.getMaxSortPriority(tempMeta
						.getParentId()) + 1;
				affecteds.add(tempAffected);
				if (null != tempTargetMetadatas
						&& tempMetaMapping.isDuplicate()) {
					tempTargetMetadatas = findChildren(targetCurPid);
				} else {
					tempTargetMetadatas = null;
				}
			}

			if (tempMeta.isFolder()) {
				tempSourceMetadatas = getSourceSubMetadata(tempMeta, fromBackup);

				copyMetadatas.addAll(tempSourceMetadatas);
			}

			if (!StringUtils.isNullOrEmpty(tempTargetMetadatas)
					&& null != (tempTarget = getSameName(tempTargetMetadatas,
							tempMeta))) {

				if (request.isOverride()) {
					if (tempMeta.isFolder() == tempTarget.isFolder()) {
						// file --> file , folder -> folder merge
						copyedMeta = copyMetadata(tempMeta, tempTarget, userId,
								destOwnerId, targetSharedRootId,
								targetSyncRootId, targetCurPid, false);

						if (!tempMeta.isFolder()) {
							// file --> file , affect -->size
							// affectSize += (tempMeta.getTotalSize() -
							// tempTarget.getTotalSize());
							filesMaps.add(new SourceTargetMap(tempMeta.getId(),
									tempTarget.getId()));
							tempAffected.addSize(tempMeta.getTotalSize()
									- tempTarget.getTotalSize());
							// update width & height
							if (copyedMeta.getWidth() != tempTarget.getWidth()
									|| copyedMeta.getHeight() != tempTarget
											.getHeight()) {
								updateAttrs.add(new MetadataAttr(copyedMeta
										.getId(), false, copyedMeta.getWidth(),
										copyedMeta.getHeight()));
							}

						} else {
							// folder --> folder, affect --> nothing, but
							// folder may be affect so add to affects
							foldersMaps
									.put(tempMeta.getId(), new FolderMapItem(
											tempTarget.getId(), true));
						}

						// temAffected.addSize(tempMeta.getTotalSize() -
						// tempTarget.getTotalSize());
						createMetas.add(copyedMeta);
						updateAlreadyReadSet(alreadyReadSet,
								request.getUserId(), tempMeta, copyedMeta);
						// deleteBackups.add(tempTarget.getId());

					} else if (request.isForceOverride()) {
						// force delete & create
						tempTarget.setFolder(tempMeta.isFolder());
						if (tempMeta.isFolder()) {
							// target is a file, affect --> folder: +1,
							// file: -1 change to folder
							tempAffected.addFileCount(-1);
							tempAffected.addFolderCount(1);
							foldersMaps.put(tempMeta.getId(),
									new FolderMapItem(tempTarget.getId()));
						} else {
							filesMaps.add(new SourceTargetMap(tempMeta.getId(),
									tempTarget.getId()));
							tempAffected.addFileCount(1 - tempTarget
									.getFilesCount());
							tempAffected.addFolderCount(-tempTarget
									.getFoldersCount());
						}

						tempAffected.addSize(tempMeta.getTotalSize()
								- tempTarget.getTotalSize());
						copyedMeta = copyMetadata(tempMeta, tempTarget, userId,
								destOwnerId, targetSharedRootId,
								targetSyncRootId, targetCurPid, false);
						// tempTarget is folder just delete self to
						deleteMetadas.add(tempTarget.getId());
						deleteBackups.add(tempTarget.getId());
						createMetas.add(copyedMeta);
						updateAlreadyReadSet(alreadyReadSet,
								request.getUserId(), tempMeta, copyedMeta);
					} else {
						throw new MetadataException(HttpStatus.FILE_EXIST);
					}
					// delete backup if exist by id
					if (copyedMeta.isEncrypted()) {
						deleteBackups.add(tempTarget.getId());
					}
				} else { // override = false

					if (tempMeta.isFolder() != tempTarget.isFolder()) {
						throw new MetadataException(HttpStatus.FILE_EXIST);
					}

					String availableName = metadataFactory.generateName(
							tempTargetMetadatas, tempTarget,
							tempTarget.isFolder(), false);

					copyedMeta = copyMetadata(tempMeta, null, userId,
							destOwnerId, targetSharedRootId, targetSyncRootId,
							targetCurPid, false);
					copyedMeta.setName(availableName.toLowerCase());
					copyedMeta.setOriginName(availableName);
					createMetas.add(copyedMeta);
					updateAlreadyReadSet(alreadyReadSet, request.getUserId(),
							tempMeta, copyedMeta);

					if (tempMeta.isFolder()) {
						// addAffect(folderAffects, tempTarget, 0);
						foldersMaps.put(tempMeta.getId(), new FolderMapItem(
								copyedMeta.getId()));

						tempAffected.addFolderCount(1);

					} else {
						// affectSize += copyedMeta.getTotalSize();
						filesMaps.add(new SourceTargetMap(tempMeta.getId(),
								copyedMeta.getId()));

						tempAffected.addFileCount(1);
						tempAffected.addSize(copyedMeta.getTotalSize());
					}

				}

				copyedMeta.setSortPriority(sortPriority);
				continue;
			}// !!

			// no same name , create
			copyedMeta = copyMetadata(tempMeta, null, userId, destOwnerId,
					targetSharedRootId, targetSyncRootId, targetCurPid, false);
			createMetas.add(copyedMeta);
			updateAlreadyReadSet(alreadyReadSet, request.getUserId(), tempMeta,
					copyedMeta);
			if (tempMeta.isFolder()) {
				// addAffect(folderAffects, tempTarget, 0);
				foldersMaps.put(tempMeta.getId(),
						new FolderMapItem(copyedMeta.getId(), false));

				tempAffected.addFolderCount(1);

			} else {
				// affectSize += copyedMeta.getTotalSize();
				filesMaps.add(new SourceTargetMap(tempMeta.getId(), copyedMeta
						.getId()));
				tempAffected.addFileCount(1);
				tempAffected.addSize(copyedMeta.getTotalSize());
			}

			copyedMeta.setSortPriority(sortPriority);

		}
		updateAll2Db(userId, fromBackup, affecteds, foldersMaps,
				createMetas, updateAttrs, deleteMetadas, deleteBackups,
				filesMaps, alreadyReadSet);
		otherLogic(targetFolderDir, targetFolderPath, destOwnerId,
				request.getDestinationSharedRootId(), pathInfo, request,
				sourceFolder.getSharedRootId(),
				ChangeReason.COPY_FOLDER.getChangeReason(),
				SystemEvent.COPY_FOLDER);
	}

	private void addModifySharedRoot(List<SharedRootMap> shareMaps,
			List<? extends BaseMetadata> metadatas) {
		shareMaps.addAll(Lists.newArrayList(Iterables.transform(metadatas,
				new Function<BaseMetadata, SharedRootMap>() {
					@Override
					public SharedRootMap apply(BaseMetadata meta) {
						return new SharedRootMap(meta.getSharedRootId(), meta
								.getId());
					}

				})));
	}

	private void updateAlreadyReadSet(Set<String> alreadyReadSet,
			String userId, BaseMetadata sourceMetadata, BaseMetadata newMetadata) {
		if (newMetadata.isFolder())
			return;
		if (alreadyReadSet.contains(newMetadata.getId()))
			return;

		if (!StringUtils.isNullOrEmpty(newMetadata.getSharedRootId())
				|| !StringUtils.isNullOrEmpty(newMetadata.getSyncRootId())) {
			boolean isAlreadyRead = true;
			if (sourceMetadata.getType().toLowerCase()
					.equals(MetadataType.LINKFILE.toString().toLowerCase())
					|| !StringUtils.isNullOrEmpty(sourceMetadata
							.getSharedRootId())) {
				isAlreadyRead = alreadyReadDao.isAlreadyRead(userId,
						sourceMetadata.getId());
			}
			if (isAlreadyRead) {
				alreadyReadSet.add(newMetadata.getId());
			}
		}
	}

	@Transactional
	private void updateAll2Db(String userId, boolean fromBackup, List<CountAffected> affecteds,
			Map<String, FolderMapItem> foldersMaps, List<Metadata> createMetas,
			List<MetadataAttr> updateAttrs, List<String> deleteMetadas,
			List<String> deleteBackups, List<SourceTargetMap> filesMaps,
			Set<String> alreadyReadSet) {
		bacthDelete(deleteMetadas, deleteBackups);

		List<BackupMetadata> backupedMetas = null;

		if (createMetas.size() == 0) {
			return;
		}
		List<MetadataAttr> creationAttrs = Lists.newArrayList(Iterables
				.transform(createMetas, new Function<Metadata, MetadataAttr>() {

					@Override
					public MetadataAttr apply(Metadata tempMeta) {
						return new MetadataAttr(tempMeta.getId(), false,
								tempMeta.getWidth(), tempMeta.getHeight());
					}
				}));

		metadataDao.batchCreateMetadata(createMetas);

		metadataAttrService.batchCreateMetadataAttr(creationAttrs);

		// update already read
		for (String metadataId : alreadyReadSet) {
			alreadyReadDao.updateAlreadyRead(metadataId, userId);
		}

		if (updateAttrs.size() > 0) {
			metadataAttrService
					.batchUpdateMetadataAttrOnOriginWithAspect(updateAttrs);
		}

		if (affecteds.size() > 0) {
			countService.updateAllParentCount(affecteds);
		}
		// if (affectSize != 0) {
		// metadataAttrService.updateRootSize(rootId, affectSize);
		// }

		// backup all bug ref file
		List<Metadata> createBackups = new ArrayList<Metadata>();
		for (Metadata createMeta : createMetas) {
			if ("".equals(createMeta.getRefId())) {
				createBackups.add(createMeta);
			}
		}
		if (!createBackups.isEmpty()) {
			backupedMetas = backupMetadataService.backupMetadata(createBackups,
					userId, createBackups.get(0).getParentId());
		}

		processStatus(filesMaps, foldersMaps, backupedMetas, userId, fromBackup);
	}

	private void otherLogic(BaseMetadata metadata, String newFullPath,
			String newOwnerId, String newSharedRootId, PathInfo pathInfo,
			RequestBase request, String shareRootId, String changeReason,
			SystemEvent event) {

		// send dynamoDB history for target file
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata
				.getSharedRootId());
		// Metadata fromMetadata =
		// metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath().toLowerCase(),
		// pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		String oldSharedUppderPath = metadataFactory
				.getSharedUpperPath(shareRootId);
		String splitPath[] = StringUtils.trimFilePathRoot(newFullPath).split(
				"/");
		if (splitPath.length > 1
				&& splitPath[0].toLowerCase().equals(
						CommonFolders.MY_SHARED_FOLDERS.toString())) {
			splitPath[0] = FilenameUtils
					.getFullPathNoEndSeparator(sharedUpperPath);
			newFullPath = Joiner.on("/").skipNulls().join(splitPath);
		}
		dynamoDBService.createActivityHistory(newFullPath,
				pathInfo.getFullOwnerPath(), sharedUpperPath,
				oldSharedUppderPath, metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(), changeReason,
				metadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());

		// send XMPP
		PathInfo newPathInfo = new PathInfo();
		newPathInfo.setFullOwnerPath(newFullPath);
		newPathInfo.setOwnerId(newOwnerId);
		newPathInfo.setSharedRootId(newSharedRootId);
		xmppService.sendToAllDevices(event, pathInfo, newPathInfo);

		// sync
		syncRelationService.sync(metadata, event);

	}

	private void processStatus(List<SourceTargetMap> fileMaps,
			Map<String, FolderMapItem> folderMaps,
			List<BackupMetadata> backupedDatas, String userId,
			boolean fromBackup) {

		Map<String, String> folderMapNormal = Maps.transformValues(folderMaps,
				new Function<FolderMapItem, String>() {
					@Override
					public String apply(FolderMapItem item) {
						return item.getTargetId();
					}
				});

		Map<String, String> allFolderMap = null;

		if (!fromBackup) {
			allFolderMap = new LinkedHashMap<String, String>();
			for (BackupMetadata backup : backupedDatas) {
				if (backup.isFolder()) {
					allFolderMap.put(backup.getOriginalIndexId(),
							backup.getId());
				} else {
					fileMaps.add(new SourceTargetMap(backup
							.getOriginalIndexId(), backup.getId()));
				}
			}

		}

		if (null == allFolderMap) {
			allFolderMap = folderMapNormal;
		} else {
			allFolderMap.putAll(folderMapNormal);
		}

		if (fileMaps.size() > 0) {
			readerFileMetadataDao.copyFileProcessingStatus(fileMaps);
			readerFileMetadataDao.copyReaderFileMetadata(fileMaps);
			readerFileMetadataDao.copyReaderFileMetadataDetail(fileMaps);
			textViewSettingDao.copyTextViewSettings(fileMaps);

		}

		profilePropertyDao.copyProfileProperty(userId, folderMapNormal);

	}

	private void bacthDelete(List<String> metadataIds, List<String> backupIds) {

		if (backupIds.size() > 0) {
			backupMetadataDao.deleteBackupsByOriginalIds(backupIds);
		}

		if (metadataIds.size() > 0) {
			metadataDao.batchDeleteMetadata(metadataIds);
			metadataAttrDao.deleteMetadataAttrs(metadataIds);
			backupMetadataService.unbackupMetadatas(metadataIds);
		}

	}

	private List<Metadata> findChildren(String parentId) {
		return metadataDao.getMetadatasByParentId(parentId);
	}

	private Metadata getSameName(List<Metadata> metadatas, BaseMetadata metadata) {
		for (Metadata item : metadatas) {
			if (item.getName().equals(metadata.getRealName())) {
				return item;
			}
		}
		return null;
	}

	private List<? extends BaseMetadata> getSourceSubMetadata(
			BaseMetadata mainMetadata, boolean fromBackup) {
		String metadataId = mainMetadata.getId();
		if (!fromBackup) {
			return distinctMetadataFromCommon(mainMetadata,
					metadataDao.getMetadatasByParentId(metadataId));
		}
		return distinctBackupMetadataFromBackupCommon(mainMetadata,
				backupMetadataDao.getBackupMetadatasByParentId(metadataId));
	}

	private List<Metadata> distinctMetadataFromCommon(
			BaseMetadata mainMetadata, List<Metadata> metadatas) {
		List<Metadata> distinctMetadatas = new ArrayList<Metadata>();
		List<String> metadataName = new ArrayList<String>();
		if (mainMetadata.getType().equals(MetadataType.COMMON.toString())) {
			for (Metadata metadata : metadatas) {
				if (!metadataName.contains(metadata.getName())) {
					distinctMetadatas.add(metadata);
					metadataName.add(metadata.getName());
				}
			}
			return distinctMetadatas;
		}
		return metadatas;
	}

	private List<BackupMetadata> distinctBackupMetadataFromBackupCommon(
			BaseMetadata mainMetadata, List<BackupMetadata> metadatas) {
		List<BackupMetadata> distinctMetadatas = new ArrayList<BackupMetadata>();
		List<String> metadataName = new ArrayList<String>();
		if (mainMetadata.getType().equals(MetadataType.BACKUPCOMMON.toString())) {
			for (BackupMetadata metadata : metadatas) {
				if (!metadataName.contains(metadata.getName())) {
					distinctMetadatas.add(metadata);
					metadataName.add(metadata.getName());
				}
			}
			return distinctMetadatas;
		}
		return metadatas;
	}

	@Override
	@Transactional
	public void updateBackupCount(
			UpdateFolderBackupCountRequest updateFolderBackupCountRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(
				updateFolderBackupCountRequest, true, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (updateFolderBackupCountRequest.getBackupCount() <= -1) {
			throw new MetadataException(HttpStatus.BACKUP_COUNT_IS_NEGATIVE);
		}

		backupMetadataService.updateBackupCount(metadata,
				updateFolderBackupCountRequest.getBackupCount());
	}

	@Override
	public GetFolderBackupCountResponse getBackupCount(
			PathRequestBase getFolderBackupCountRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(
				getFolderBackupCountRequest, true, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());

		if (metadata == null || !metadata.isFolder()) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}

		// when backupCount is negative, then count = upperFolder's count
		GetFolderBackupCountResponse getFolderBackupCountResponse = new GetFolderBackupCountResponse();
		int backupCount = 0;
		if (metadata.getBackupCount() < 0) {
			List<Metadata> allUpperLevelMetadatas = metadataFactory
					.getAllUpperLevelMetadatas(metadata);
			for (Metadata upperLevelMetadata : allUpperLevelMetadatas) {
				if (upperLevelMetadata.getBackupCount() >= 0) {
					backupCount = upperLevelMetadata.getBackupCount();
					break;
				}
			}
		}
		int count = metadata.getBackupCount() >= 0 ? metadata.getBackupCount()
				: backupCount;
		if (metadata.getType().equals(MetadataType.BACKUP.toString())
				|| metadata.getType().equals(
						MetadataType.BACKUPCOMMON.toString())) {
			count = 0;
		}
		getFolderBackupCountResponse.setBackupCount(count);

		return getFolderBackupCountResponse;
	}

	@Override
	@Transactional
	public Boolean updatePriority(UpdatePriorityRequest updatePriorityRequest)
			throws MetadataException {
		PathInfo parentPathInfo = pathFactory.parsePathInfo(
				updatePriorityRequest, true, true);
		Metadata parentFolder = metadataDao.getMetadataByPath(
				parentPathInfo.getFullOwnerPath(),
				parentPathInfo.getDeviceUid(), "");

		for (PriorityInfo priority : updatePriorityRequest.getPriority()) {
			String fullSourcePath = StringUtils.isNullOrEmpty(priority
					.getSourcePath()) ? "" : parentPathInfo.getOwnerId() + "/"
					+ priority.getSourcePath();
			String deviceUid = pathFactory.fetchDeviceUid(priority
					.getSourcePath());
			Metadata metadata = metadataDao.getMetadataByCriteria(
					parentFolder.getId(), priority.getName(), deviceUid,
					fullSourcePath);

			if (metadata == null) {
				throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
			}

			metadata.setSortPriority(priority.getIndex());

			folderDao.updatePriority(metadata);
			backupMetadataService.updateBackup(metadata);
		}
		// activity history
		String sharedUpperPath = metadataFactory
				.getSharedUpperPath(updatePriorityRequest.getSharedRootId());
		dynamoDBService.createActivityHistory(
				parentPathInfo.getFullOwnerPath(), "", sharedUpperPath, "",
				parentFolder.getId(), updatePriorityRequest.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.ARRANGE_FOLDER.toString(),
				updatePriorityRequest.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());

		return true;
	}

	@Override
	@Transactional
	public void unlinkFolder(PathRequestBase unlinkFolderRequest)
			throws MetadataException {
		pathFactory.checkCanUnlinkFolder(unlinkFolderRequest.getPath());

		PathInfo pathInfo = pathFactory.parsePathInfo(unlinkFolderRequest,
				true, true);

		List<Metadata> files = metadataDao.getAllFilesInLinkFolder(
				pathInfo.getFullSourcePath(), false);

		for (Metadata file : files) {
			fileService.deleteFile(file);
		}

		folderDao.setFolderUnlinked(unlinkFolderRequest.getUserId(),
				pathInfo.getFullSourcePath());

		dynamoDBService.createActivityHistory(pathInfo.getFullSourcePath(), "",
				"", "", "", pathInfo.getUserId(), DateUtils.nowUTCDateTime(),
				ChangeReason.UNLINK_FOLDER.toString(), "",
				ActivityHistoryLevel.NORMAL.toString());
	}

	@Override
	@Transactional
	public void linkFolder(PathRequestBase linkFolderRequest)
			throws MetadataException {
		pathFactory.checkCanUnlinkFolder(linkFolderRequest.getPath());

		PathInfo pathInfo = pathFactory.parsePathInfo(linkFolderRequest, true,
				true);

		folderDao.setFolderLinked(linkFolderRequest.getUserId(),
				pathInfo.getFullSourcePath());
		//
		dynamoDBService.createActivityHistory(pathInfo.getFullSourcePath(), "",
				"", "", "", pathInfo.getUserId(), DateUtils.nowUTCDateTime(),
				ChangeReason.LINK_FOLDER.toString(), "",
				ActivityHistoryLevel.NORMAL.toString());
	}

	@Override
	public LinkFoldersResponse linkFolders(LinkFoldersRequest linkFolderRequest)
			throws MetadataException {
		String path = linkFolderRequest.getPath();

		if (path == null) {
			throw new MetadataException(HttpStatus.PARAM_NOT_EXIST);
		}

		if (!path.equals("")) {
			path = linkFolderRequest.getUserId() + "/" + path;
		}

		return getQueryFolders(linkFolderRequest, SyncType.SYNC, path);
	}

	@Override
	public LinkFoldersResponse unlinkFolders(RequestBase unlinkFoldersRequest)
			throws MetadataException {
		return getQueryFolders(unlinkFoldersRequest, SyncType.UNSYNC, "");
	}

	@Override
	@Transactional
	public Boolean updateFolderNote(
			UpdateFolderNoteRequest updateFolderNoteRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateFolderNoteRequest,
				true, true);
		BaseMetadata metadata = metadataFactory.getFolderByPathInfo(pathInfo);

		metadata.setNote(updateFolderNoteRequest.getNote());
		long modifiedAt = DateUtils.nowUTCTimestamp();
		if (metadata instanceof Metadata) {
			metadataDao.updateNote(metadata.getId(), metadata.getNote(),
					modifiedAt, updateFolderNoteRequest.getUserId());
			BackupMetadata backupMetadata = backupMetadataDao
					.getLatestBackup(metadata.getId());
			if (backupMetadata != null) {
				backupMetadataDao.updateBackupNote(backupMetadata.getId(),
						metadata.getNote(), modifiedAt,
						updateFolderNoteRequest.getUserId());
			}
		} else {
			backupMetadataDao.updateBackupNote(metadata.getId(),
					metadata.getNote(), modifiedAt,
					updateFolderNoteRequest.getUserId());
		}

		// send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata
				.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.UPDATE_NOTE.toString(),
				metadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.UPDATE_NOTE, pathInfo);
		// sync
		if (!syncRelationService.inCallerList()) {
			syncRelationService.sync(updateFolderNoteRequest, metadata,
					SystemEvent.UPDATE_NOTE);
		}
		return true;
	}

	@Override
	public GetFolderNoteResponse getFolderNote(
			PathRequestBase getFolderNoteRequest) throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(getFolderNoteRequest,
				true, true);
		BaseMetadata metadata = metadataFactory.getFolderByPathInfo(pathInfo);
		GetFolderNoteResponse response = new GetFolderNoteResponse();
		response.setNote(metadata.getNote());
		return response;
	}

	@Override
	@Transactional
	public void deleteFolderBackup(
			DeleteFolderBackupRequest deleteFolderBackupRequest)
			throws MetadataException {

		if (null == deleteFolderBackupRequest
				|| null == deleteFolderBackupRequest.getPath()) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		String ownerId = deleteFolderBackupRequest.getUserId();

		// root
		if (deleteFolderBackupRequest.getPath().equals("")
				|| deleteFolderBackupRequest.getPath().equals("/")) {
			backupMetadataService.deleteAllBackup(ownerId);
		} else if (deleteFolderBackupRequest.getPath().equalsIgnoreCase(
				CommonFolders.MY_BACKUP_DATA.toString())
				|| StringUtils.startsWith(deleteFolderBackupRequest.getPath(),
						CommonFolders.MY_BACKUP_DATA.toString() + "/")) {

			String folderId = backupMetadataService.getBackupIndexByPath(
					StringUtils.trimFilePathRoot(deleteFolderBackupRequest
							.getPath()), ownerId);

			if (StringUtils.isNullOrEmpty(folderId)) {
				throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
			}

			if (deleteFolderBackupRequest.isRecursiveClean()) {
				backupMetadataService.deleteFolderAllBackup(ownerId, folderId);
			} else {
				backupMetadataService.deleteFolderBackup(ownerId, folderId);
			}
		}

		// return null;
	}

	@Override
	public List<GetSubBackupCountResponse> getSubBackupCount(
			PathRequestBase getSubBackupCountRequest) throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(getSubBackupCountRequest,
				true, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (!metadata.isFolder()) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		int parentBackupCount = backupMetadataService.getBackupCount(metadata);
		List<Metadata> childMetadatas = metadataDao
				.getMetadatasByParentId(metadata.getId());
		List<Metadata> extensionMetadatas = backupMetadataService
				.getBackupCountByExtension(pathInfo.getOwnerId(),
						childMetadatas);
		List<GetSubBackupCountResponse> result = new ArrayList<GetSubBackupCountResponse>();
		for (Metadata extensionMetadata : extensionMetadatas) {
			GetSubBackupCountResponse getSubBackupCountResponse = new GetSubBackupCountResponse();
			getSubBackupCountResponse
					.setName(extensionMetadata.getOriginName());
			getSubBackupCountResponse.setSourcePath(extensionMetadata
					.getSourcePath());
			getSubBackupCountResponse.setBackupCount(extensionMetadata
					.getBackupCount() == -1 ? parentBackupCount
					: extensionMetadata.getBackupCount());
			result.add(getSubBackupCountResponse);
		}

		return result;
	}

	@Override
	@Transactional
	public void zip(ZipRequest zipRequest) throws MetadataException {
		// check target path : 1. not common
		pathFactory.checkCanZipUnzip(zipRequest.getTargetPath());
		// check target path : 2. not exist
		PathRequestBase target = new PathRequestBase();
		target.setPath(zipRequest.getTargetPath());
		target.setOwnerId(zipRequest.getTargetOwnerId());
		target.setSharedRootId(zipRequest.getTargetSharedRootId());
		target.setUserId(zipRequest.getUserId());
		PathInfo targetPathInfo = pathFactory.parsePathInfo(target, true, true);
		Metadata targetMetadata = metadataDao.getMetadataByPath(
				targetPathInfo.getFullOwnerPath(), "", "");
		if (targetMetadata != null) {
			throw new MetadataException(HttpStatus.FILE_EXIST);
		}
		// check target path : 3. parent folder exist
		Metadata targetParent = metadataDao.getMetadataByPath(FilenameUtils
				.getFullPathNoEndSeparator(targetPathInfo.getFullOwnerPath()),
				"", "");
		if (targetParent == null) {
			throw new MetadataException(HttpStatus.CANT_ZIP_UNZIP);
		}

		for (ZipSource zipSource : zipRequest.getZipSource()) {
			// check source : 1. not common
			pathFactory.checkCanZipUnzip(zipSource.getPath());
			// check source : 2. =/= target
			zipSource.setUserId(zipRequest.getUserId());
			PathInfo sourcePathInfo = pathFactory.parsePathInfo(zipSource,
					true, true);
			if (sourcePathInfo.getFullOwnerPath().equals(
					targetPathInfo.getFullOwnerPath())) {
				throw new MetadataException(HttpStatus.CANT_ZIP_UNZIP);
			}
			// check source : 3. exist
			Metadata sourceMetadata = metadataDao.getMetadataByPath(
					sourcePathInfo.getFullOwnerPath(),
					sourcePathInfo.getDeviceUid(),
					sourcePathInfo.getFullSourcePath());
			if (sourceMetadata == null
					|| sourceMetadata.isFolder() != zipSource.isFolder()) {
				throw new MetadataException(HttpStatus.ZIP_SOURCE_NOT_EXIST);
			}
			// check source : 4. in process(have no block_id)
			if (!sourceMetadata.isFolder()
					&& sourceMetadata.getBlockId().equals("")) {
				throw new MetadataException(HttpStatus.CANT_ZIP_UNZIP);
			}
		}

		// create target
		UpdateFileRequest createFileRequest = new UpdateFileRequest();
		createFileRequest.setUserId(zipRequest.getUserId());
		createFileRequest.setPath(zipRequest.getTargetPath());
		createFileRequest.setOwnerId(zipRequest.getTargetOwnerId());
		createFileRequest.setSharedRootId(zipRequest.getTargetSharedRootId());
		createFileRequest.setSize(0L);
		createFileRequest.setBlockId("");
		createFileRequest.setToken(zipRequest.getToken());
		createFileRequest.setDeviceId(zipRequest.getDeviceId());
		fileService.createFile(createFileRequest);
		// update processing status on target
		targetMetadata = metadataDao.getMetadataByPath(
				targetPathInfo.getFullOwnerPath(), "", "");
		FileProcessingStatus fileProcessingStatus = new FileProcessingStatus(
				targetMetadata.getId(), ProcessingStatus.WAITING, 0, "",
				ProcessAction.ZIP);
		fileProcessingStatusDao
				.updateFileProcessingStatus(fileProcessingStatus);
		// send zip mq
		mqService.sendForZip(zipRequest);
	}

	@Override
	@Transactional
	public void unzip(UnzipRequest unzipRequest) throws MetadataException {
		// check source : 1. not common
		pathFactory.checkCanZipUnzip(unzipRequest.getPath());
		// check source : 2. exist
		PathInfo pathInfo = pathFactory.parsePathInfo(unzipRequest, true, true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), "", "");
		if (metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		// check source : 3. in process(have no block_id)
		if (metadata.getBlockId().equals("")) {
			throw new MetadataException(HttpStatus.CANT_ZIP_UNZIP);
		}
		// check target(folder): 1. not common
		pathFactory.checkCanZipUnzip(unzipRequest.getTargetPath());
		// check target(folder): 2. if not exist => create
		PathRequestBase folderRequest = new PathRequestBase();
		folderRequest.setUserId(unzipRequest.getUserId());
		folderRequest.setPath(unzipRequest.getTargetPath());
		folderRequest.setOwnerId(unzipRequest.getTargetOwnerId());

		folderRequest.setSharedRootId(unzipRequest.getTargetSharedRootId());
		pathInfo = pathFactory.parsePathInfo(folderRequest, true, true);

		metadata = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
				"", "");
		if (metadata == null) {
			createFolder(folderRequest);
			metadata = metadataDao.getMetadataByPath(
					pathInfo.getFullOwnerPath(), "", "");
		} else {
			throw new MetadataException(HttpStatus.FOLDER_EXIST);
		}
		// update processing status on target
		FileProcessingStatus fileProcessingStatus = new FileProcessingStatus(
				metadata.getId(), ProcessingStatus.WAITING, 0, "",
				ProcessAction.UNZIP);
		fileProcessingStatusDao
				.updateFileProcessingStatus(fileProcessingStatus);
		// send unzip mq
		mqService.sendForUnzip(unzipRequest);
	}

	@Override
	@Transactional
	public void addGlobalIcon(AddGlobalIconRequest addGlobalIconRequest)
			throws MetadataException {

		int groupId = getIconGorupId(addGlobalIconRequest.getGroupName(),
				addGlobalIconRequest.getUserId(),
				addGlobalIconRequest.isFolder());
		if (groupId < 0) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		List<GlobalIcon> userIcon = folderDao
				.getGlobalIconByUser(addGlobalIconRequest.getUserId());
		for (GlobalIcon element : userIcon) {
			if (element.getBlockId().equals(addGlobalIconRequest.getBlockId())
					&& element.getGroupId() == groupId) {
				throw new MetadataException(HttpStatus.FILE_EXIST);
			}
		}

		List<GlobalIcon> globalIcon = folderDao.getGlobalIconByUser("");
		for (GlobalIcon element : globalIcon) {
			if (element.getBlockId().equals(addGlobalIconRequest.getBlockId())) {
				throw new MetadataException(HttpStatus.FILE_EXIST);
			}
		}

		GlobalIcon addGlobalIcon = new GlobalIcon();
		addGlobalIcon.setBlockId(addGlobalIconRequest.getBlockId());
		addGlobalIcon.setDefaultIcon(false);
		addGlobalIcon.setUser(addGlobalIconRequest.getUserId());
		addGlobalIcon.setType("");
		addGlobalIcon.setSystem(false);
		addGlobalIcon.setGroupId(groupId);
		addGlobalIcon.setFolder(addGlobalIconRequest.isFolder());
		List<GlobalIcon> globalicons = new ArrayList<GlobalIcon>();
		globalicons.add(addGlobalIcon);
		folderDao.addGlobalIcon(globalicons);
	}

	private int getIconGorupId(String groupName, String userId, boolean isFolder) {
		int groupId = -1;
		if ("".equals(groupName)) {
			groupId = 0;
		} else {
			Integer groupIdObj = folderDao.getGlobalIconGroupId(userId,
					isFolder, groupName);
			if (groupIdObj != null) {
				groupId = groupIdObj.intValue();
			}
		}
		return groupId;
	}

	@Override
	@Transactional
	public void deleteGlobalIcon(DeleteGlobalIconRequest deleteGlobalIconRequest)
			throws MetadataException {
		List<GlobalIcon> deleteIcons = new ArrayList<GlobalIcon>();
		List<GlobalIcon> userIcons = folderDao
				.getGlobalIconByUser(deleteGlobalIconRequest.getUserId());
		int groupId = getIconGorupId(deleteGlobalIconRequest.getGroupName(),
				deleteGlobalIconRequest.getUserId(),
				deleteGlobalIconRequest.isFolder());
		if (groupId < 0) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		for (GlobalIcon userIcon : userIcons) {
			if (!userIcon.getSystem().booleanValue()
					&& userIcon.getFolder().booleanValue() == deleteGlobalIconRequest
							.isFolder()) {
				for (String iconBlockId : deleteGlobalIconRequest.getBlockIds()) {
					if (userIcon.getBlockId().equals(iconBlockId)
							&& userIcon.getGroupId() == groupId
							&& userIcon.getFolder().booleanValue() == deleteGlobalIconRequest
									.isFolder()) {
						deleteIcons.add(userIcon);
					}
				}
			}
		}

		HashSet<GlobalIcon> hs = new HashSet<GlobalIcon>();
		hs.addAll(deleteIcons);
		List<GlobalIcon> deleteIconsDistinct = new ArrayList<GlobalIcon>();
		deleteIconsDistinct.addAll(hs);
		folderDao.deleteGlobalIcon(deleteIconsDistinct);

		List<GlobalIcon> cleanMetadataIcon = new ArrayList<GlobalIcon>();
		List<GlobalIcon> removedUserIcons = new ArrayList<GlobalIcon>();
		userIcons.removeAll(deleteIconsDistinct);
		removedUserIcons = userIcons;

		for (GlobalIcon deleteIcon : deleteIconsDistinct) {
			boolean findLeftIconBlockId = false;
			for (GlobalIcon userIcon : removedUserIcons) {
				if (userIcon.getBlockId().equals(deleteIcon.getBlockId())
						&& userIcon.getFolder().booleanValue() == deleteGlobalIconRequest
								.isFolder()) {
					findLeftIconBlockId = true;
					break;
				}
			}
			if (!findLeftIconBlockId) {
				cleanMetadataIcon.add(deleteIcon);
			}
		}

		cleanMetadataIcon(cleanMetadataIcon,
				deleteGlobalIconRequest.getUserId());

	}

	@Transactional
	private void cleanMetadataIcon(List<GlobalIcon> cleanIcons, String userId) {
		String rootId = metadataDao.getRootMetadataId(userId);
		Metadata rootMetadata = metadataDao.getMetadata(rootId);
		List<Metadata> allMetadata = metadataFactory
				.getAllSubFoldersAndFiles(rootMetadata);
		List<Metadata> removeIconMetadata = new ArrayList<Metadata>();
		for (Metadata element : allMetadata) {
			for (GlobalIcon icon : cleanIcons) {
				if (element.getIconBlockId().equals(icon.getBlockId())
						&& element.isFolder() == icon.getFolder()) {
					removeIconMetadata.add(element);
				}
			}
		}
		metadataDao.batchCleanMetadataIcon(removeIconMetadata);
	}

	@Override
	@Transactional
	public void cleanUnuseGlobalIcon(RequestBase cleanUnuseGlboallIconRequest)
			throws MetadataException {
		List<GlobalIcon> userIcon = folderDao
				.getGlobalIconByUser(cleanUnuseGlboallIconRequest.getUserId());
		String rootId = metadataDao
				.getRootMetadataId(cleanUnuseGlboallIconRequest.getUserId());
		Metadata rootMetadata = metadataDao.getMetadata(rootId);
		List<Metadata> allMetadata = metadataFactory
				.getAllSubFoldersAndFiles(rootMetadata);
		List<GlobalIcon> deleteicons = new ArrayList<GlobalIcon>();

		for (GlobalIcon element : userIcon) {
			if (!element.getDefaultIcon().booleanValue()) {
				boolean findUseIcon = false;
				for (Metadata metadata : allMetadata) {
					if (element.getBlockId().equals(metadata.getIconBlockId())
							&& element.getFolder().booleanValue() == metadata
									.isFolder()) {
						findUseIcon = true;
						break;
					}
				}
				if (!findUseIcon) {
					deleteicons.add(element);
				}
			}
		}
		folderDao.deleteGlobalIcon(deleteicons);
	}

	@Override
	@Transactional
	public void resetGlobalIcon(ResetGlobalIconRequest resetGlobalIconRequest)
			throws MetadataException {
		resetGlobalIconProcess(resetGlobalIconRequest.getUserId(),
				resetGlobalIconRequest.isFolder());
	}

	@Override
	@Transactional
	public void resetGlobalIconByUser(String userId) {
		resetGlobalIconProcess(userId, true);
		resetGlobalIconProcess(userId, false);
	}

	@Transactional
	private void resetGlobalIconProcess(String userId, boolean isFolder) {
		List<GlobalIcon> deleteIcons = new ArrayList<GlobalIcon>();
		List<GlobalIcon> userIcon = folderDao.getGlobalIconByUser(userId);
		for (GlobalIcon element : userIcon) {
			if (element.getDefaultIcon().booleanValue()
					&& element.getFolder().booleanValue() == isFolder) {
				deleteIcons.add(element);
			}
		}
		if (!deleteIcons.isEmpty()) {
			folderDao.deleteGlobalIcon(deleteIcons);
		}
		List<GlobalIcon> addIcons = new ArrayList<GlobalIcon>();
		List<GlobalIcon> globalIcons = folderDao.getGlobalIconByUser("");
		for (GlobalIcon globalIcon : globalIcons) {
			if (globalIcon.getDefaultIcon().booleanValue()
					&& globalIcon.getFolder().booleanValue() == isFolder) {
				globalIcon.setUser(userId);
				addIcons.add(globalIcon);
			}
		}
		folderDao.addGlobalIcon(addIcons);
	}

	@Override
	public GetGlobalIconResponse getGlobalIcon(
			GetGlobalIconRequest getGlobalIconRequest) throws MetadataException {
		int groupId = getIconGorupId(getGlobalIconRequest.getGroupName(),
				getGlobalIconRequest.getUserId(),
				getGlobalIconRequest.getFolder());
		if (groupId < 0) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		GetGlobalIconResponse globalIconResponse = new GetGlobalIconResponse();
		List<GlobalIcon> globalIcons = folderDao.getGlobalIcon(
				getGlobalIconRequest.getUserId(),
				getGlobalIconRequest.isIncludeUserDefinedIcon(),
				getGlobalIconRequest.getFolder(), groupId);
		globalIconResponse.getFiles().addAll(globalIcons);

		List<GlobalIconGroup> globalIconGroupRemoveIdDisplay = new ArrayList<GlobalIconGroup>();
		if (groupId == 0) {
			List<GlobalIconGroup> globalIconGroups = folderDao
					.getGlobalIconGroup(getGlobalIconRequest.getUserId(),
							getGlobalIconRequest.getFolder());

			if (globalIconGroups != null && !globalIconGroups.isEmpty()) {
				for (GlobalIconGroup globalIconGroup : globalIconGroups) {
					globalIconGroup.setId(null);
					globalIconGroupRemoveIdDisplay.add(globalIconGroup);
				}
			}
			globalIconResponse.getFolders().addAll(
					globalIconGroupRemoveIdDisplay);
		}
		return globalIconResponse;
	}

	@Override
	@Transactional
	public void updateIcon(UpdateIconRequest updateIconRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateIconRequest, true,
				true);
		Metadata metadata = metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath());
		if (metadata == null) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}
		if (metadata.isEncrypted()) {
			throw new MetadataException(HttpStatus.VIOLATE_ENCRYPTION_RULE);
		}
		boolean isIconFound = false;
		List<GlobalIcon> userIcon = folderDao
				.getGlobalIconByUser(updateIconRequest.getUserId());
		for (GlobalIcon element : userIcon) {
			if (element.getBlockId().equals(updateIconRequest.getBlockId())) {
				isIconFound = true;
			}
		}
		if ((!isIconFound && !"".equals(updateIconRequest.getBlockId()))
				|| metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}

		if (updateIconRequest.getIconText().length() > 20) {
			throw new MetadataException(HttpStatus.STRING_IS_TOO_LONG);
		}
		for (ExtraIcon extraIcon : ExtraIcon.values()) {
			if (extraIcon.getExtension().equals(
					FilenameUtils.getExtension(metadata.getName()))
					&& extraIcon.getBlockId().equals(
							updateIconRequest.getBlockId())) {
				updateIconRequest.setBlockId("");
				break;
			}
		}
		metadataDao.updateMetadataIcon(metadata.getId(),
				updateIconRequest.getBlockId(),
				updateIconRequest.getIconText(),
				updateIconRequest.getIconTextColor(),
				updateIconRequest.getIconTextStyle());
		// send dynamoDB history for target folder or file
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata
				.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(), ChangeReason.SET_ICON.toString(),
				metadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
		// sync
		if (!syncRelationService.isCaller()) {
			syncRelationService.sync(updateIconRequest, metadata,
					SystemEvent.SET_ICON);
		}
	}

	@Override
	public void moveGlobalIcon(MoveGlobalIconRequest moveGlobalIconRequest) {
		if (StringUtils.isNullOrEmpty(moveGlobalIconRequest.getBlockId())) {
			throw new MetadataException(HttpStatus.ICON_EXIST);
		}
		int groupId = getIconGorupId(moveGlobalIconRequest.getGroupName(),
				moveGlobalIconRequest.getUserId(),
				moveGlobalIconRequest.isFolder());
		int destinationGroupId = getIconGorupId(
				moveGlobalIconRequest.getDestination(),
				moveGlobalIconRequest.getUserId(),
				moveGlobalIconRequest.isFolder());
		if (groupId == -1 || destinationGroupId == -1
				|| groupId == destinationGroupId) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}

		boolean findSourceBlockId = false;
		boolean findTargetBlockId = false;
		int iconId = -1;
		List<GlobalIcon> userIcon = folderDao
				.getGlobalIconByUser(moveGlobalIconRequest.getUserId());
		for (GlobalIcon element : userIcon) {
			if (!element.getSystem().booleanValue()
					&& element.getFolder().booleanValue() == moveGlobalIconRequest
							.isFolder()
					&& element.getBlockId().equals(
							moveGlobalIconRequest.getBlockId())
					&& element.getGroupId() == groupId) {
				findSourceBlockId = true;
				iconId = element.getId();
			}
			if (!element.getSystem().booleanValue()
					&& element.getFolder().booleanValue() == moveGlobalIconRequest
							.isFolder()
					&& element.getBlockId().equals(
							moveGlobalIconRequest.getBlockId())
					&& element.getGroupId() == destinationGroupId) {
				findTargetBlockId = true;
			}
		}
		if (!findSourceBlockId) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (findTargetBlockId) {
			throw new MetadataException(HttpStatus.FILE_EXIST);
		}

		folderDao.moveGlobalIcon(iconId, destinationGroupId);
	}

	@Override
	public void createGlobalIconGroup(
			CreateGlobalIconGroupRequest createGlobalIconGroupRequest) {
		int groupId = getIconGorupId(createGlobalIconGroupRequest.getName(),
				createGlobalIconGroupRequest.getUserId(),
				createGlobalIconGroupRequest.isFolder());
		if (groupId > 0) {
			throw new MetadataException(HttpStatus.FILE_EXIST);
		}
		folderDao.createGlobalIconGroup(
				createGlobalIconGroupRequest.getUserId(),
				createGlobalIconGroupRequest.getName(),
				createGlobalIconGroupRequest.isFolder());
	}

	@Override
	@Transactional
	public void deleteGlobalIconGroup(
			DeleteGlobalIconGroupRequest deleteGlobalIconGroupRequest) {
		int groupId = getIconGorupId(deleteGlobalIconGroupRequest.getName(),
				deleteGlobalIconGroupRequest.getUserId(),
				deleteGlobalIconGroupRequest.getFolder());
		if (groupId == 0) {
			throw new MetadataException(HttpStatus.CANT_DELETE_THIS_FILE);
		} else if (groupId < 0) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}

		List<String> deleteIconsBlockIdInGroup = new ArrayList<String>();
		List<GlobalIcon> userIcon = folderDao
				.getGlobalIconByUser(deleteGlobalIconGroupRequest.getUserId());
		for (GlobalIcon element : userIcon) {
			if (element.getGroupId() == groupId) {
				deleteIconsBlockIdInGroup.add(element.getBlockId());
			}
		}
		if (!deleteIconsBlockIdInGroup.isEmpty()) {
			DeleteGlobalIconRequest deleteGlobalIconRequest = new DeleteGlobalIconRequest();
			deleteGlobalIconRequest.setBlockIds(deleteIconsBlockIdInGroup);
			deleteGlobalIconRequest.setGroupName(deleteGlobalIconGroupRequest
					.getName());
			deleteGlobalIconRequest.setUserId(deleteGlobalIconGroupRequest
					.getUserId());
			deleteGlobalIconRequest.setFolder(deleteGlobalIconGroupRequest
					.getFolder());
			deleteGlobalIcon(deleteGlobalIconRequest);
		}
		folderDao.deleteGlobalIconGroup(groupId);
	}

	@Override
	public void renameGlobalIconGroup(
			RenameGlobalIconGroupRequest renameGlobalIconGroupRequest) {
		int groupId = getIconGorupId(renameGlobalIconGroupRequest.getName(),
				renameGlobalIconGroupRequest.getUserId(),
				renameGlobalIconGroupRequest.isFolder());
		int reNameGroupId = getIconGorupId(
				renameGlobalIconGroupRequest.getNewName(),
				renameGlobalIconGroupRequest.getUserId(),
				renameGlobalIconGroupRequest.isFolder());
		if (groupId < 0) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		if (reNameGroupId >= 0) {
			throw new MetadataException(HttpStatus.FILE_EXIST);
		}
		GlobalIconGroup globalIconGroup = new GlobalIconGroup();
		globalIconGroup.setId(groupId);
		globalIconGroup.setName(renameGlobalIconGroupRequest.getNewName());
		folderDao.updateGlobalIconGroup(globalIconGroup);
	}

	@Override
	@Transactional
	public void encryptFolder(EncryptRequest encryptRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(encryptRequest, true,
				true);
		BaseMetadata metadata = metadataFactory.getFolderByPathInfo(pathInfo);
		// check: encryption rule
		checkCanEncryptFolder(metadata);
		// check: path is under processing
		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao
				.getFileProcessingStatus(metadata.getId());
		if (fileProcessingStatus != null
				&& fileProcessingStatus.isUnderProcessing()) {
			throw new MetadataException(HttpStatus.UNDER_PROCESSING);
		}
		// update processing status
		fileProcessingStatus = new FileProcessingStatus(metadata.getId(),
				ProcessingStatus.WAITING, 0, "", ProcessAction.ENCRYPT);
		fileProcessingStatusDao
				.updateFileProcessingStatus(fileProcessingStatus);
		// send encrypt MQ
		mqService.sendForEncrypt(metadata.isFolder(), encryptRequest);
		// send mail
		if (encryptRequest.isSendNotification()) {
			emailService.sendEncryptionNotificationMail(encryptRequest);
		}
	}

	private void checkCanEncryptFolder(BaseMetadata metadata) {
		// not under common folders and shared folders
		String path = (metadata.getPath() != null ? metadata.getPath()
				: metadataFactory.getMetadataOriginalPath(metadata));
		path = path.toLowerCase();
		for (CommonFolders commonFolder : CommonFolders.values()) {
			if (commonFolder != CommonFolders.MY_SHARED_FOLDERS
					&& commonFolder != CommonFolders.My_STORAGE_DATA
					&& (path.equals(commonFolder.toString()) || path
							.startsWith(commonFolder.toString().concat("/")))) {
				throw new MetadataException(HttpStatus.CANT_ENCRYPT);
			}
		}
		// not contains shared folder or sync folder
		List<Metadata> subFolderList = metadataFactory
				.getAllSubFolders((Metadata) metadata);
		for (Metadata subFolder : subFolderList) {
			if ((!StringUtils.isNullOrEmpty(subFolder.getSharedRootId()) && subFolder
					.getSharedRootId().equals(subFolder.getId()))
					|| !StringUtils.isNullOrEmpty(subFolder.getSyncRootId())) {
				throw new MetadataException(HttpStatus.CANT_ENCRYPT);
			}
		}
	}

	@Override
	@Transactional
	public void afterEncrypt(UpdateFileRequest updateFileRequest) {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateFileRequest, true,
				true);
		Metadata metadata = (Metadata) metadataFactory
				.getFolderByPathInfo(pathInfo);
		// delete original folder
		metadataDao.deleteMetadata(metadata.getId());
		// delete attr
		metadataAttrService.deleteMetadataAttr(metadata.getId());
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.deleteFolder(new CountAffected(metadata));
		countService.updateAllParentCount(countManageModel);

		// delete original backup
		BackupMetadata backup = backupMetadataDao.getLatestBackup(metadata
				.getId());
		if (backup != null) {
			backupMetadataDao.deleteBackupById(backup.getId());
			// delete attr
			metadataAttrService.deleteMetadataAttr(backup.getId());
			// update parents' size/count
			countManageModel = new CountManageModel();
			if (backup != null) {
				countManageModel.deleteFolder(new CountAffected(backup));
			}
			countService.updateAllParentCount(countManageModel);
			backupMetadataDao.deleteBackupById(backup.getId());
			// delete attr
			metadataAttrService.deleteMetadataAttr(backup.getId());
			// update parents' size/count
			CountManageModel countManageModelBackup = new CountManageModel();
			countManageModelBackup.deleteFolder(new CountAffected(backup));
			countService.updateAllParentCountBackup(countManageModelBackup);
		}

		// create encrypted file w/o backup
		updateFileRequest.setNote(metadata.getNote());
		fileService.createFile(updateFileRequest);
	}

	@Override
	@Transactional
	public void afterDecrypt(
	/* BulkCreateFileOrFolderRequest bulkCreateFileOrFolderRequest */
	BulkActionRequest<BulkCreateRequestItem> bulkActionRequest) {
		PathRequestBase pathRequestBase = bulkActionRequest.get(0);
		pathRequestBase.fromRequestBase(bulkActionRequest);
		PathInfo pathInfo = pathFactory.parsePathInfo(pathRequestBase, true,
				true);
		Metadata metadata = (Metadata) metadataFactory
				.getFileByPathInfo(pathInfo);
		// delete encrypted file
		metadataDao.deleteMetadata(metadata.getId());
		// delete attr
		metadataAttrService.deleteMetadataAttr(metadata.getId());
		// update parents' size/count
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.deleteFile(metadata.getParentId(), metadata.getSize());
		countService.updateAllParentCount(countManageModel);

		// create original folders/files
		bulkActionRequest.get(0).setNote(metadata.getNote());

		folderController.bulkCreate(bulkActionRequest);
	}

	private LinkFoldersResponse getQueryFolders(RequestBase request,
			SyncType type, String fullSourcePath) {
		List<MobileFolders> linkFolders = folderDao.getQueryFolders(
				request.getUserId(), type, fullSourcePath);

		GetAllDeviceRequest getAllDeviceRequest = new GetAllDeviceRequest();
		getAllDeviceRequest.setUserId(request.getUserId());
		getAllDeviceRequest.setToken(request.getToken());
		List<GetDeviceResponse> devices = deviceService.getAllDevice(
				getAllDeviceRequest).getDevices();

		HashMap<String, String> mapDeviceNames = new HashMap<String, String>();
		for (GetDeviceResponse device : devices) {
			mapDeviceNames.put(device.getDeviceUid(), device.getName());
		}

		List<LinkFolderResponse> folders = new ArrayList<LinkFolderResponse>();
		for (int i = 0; i < linkFolders.size(); i++) {
			MobileFolders linkFolder = linkFolders.get(i);
			LinkFolderResponse folder = new LinkFolderResponse();

			if (mapDeviceNames.containsKey(linkFolder.getDeviceUid())) {
				String sourcePath = "";
				if (linkFolder.getFullSourcePath().startsWith(
						linkFolder.getUserId() + "/")) {
					sourcePath = linkFolder.getFullSourcePath().substring(
							linkFolder.getUserId().length() + 1);
				}

				folder.setDeviceName(mapDeviceNames.get(linkFolder
						.getDeviceUid()));
				folder.setDeviceUid(linkFolder.getDeviceUid());
				folder.setName(FilenameUtils.getName(sourcePath));
				folder.setPath(sourcePath);
				folder.setType(linkFolder.getStatus().toString());
				folders.add(folder);
			}
		}

		LinkFoldersResponse response = new LinkFoldersResponse();
		response.setFolders(folders);

		return response;
	}

	private Metadata copyMetadata(BaseMetadata sourceMetadata,
			Metadata targetMetadata, String userId, String targetOwnerId,
			String sharedRootId, String syncRootId, String targetParentId,
			boolean isMove) {
		// priority set self
		Metadata resultMetadata = new Metadata();
		if (targetMetadata == null) {
			resultMetadata.fromBaseMetadata(sourceMetadata);
			resultMetadata.setId(StringUtils.getUUID());

			resultMetadata.setCreatedAt(DateUtils.nowUTCTimestamp());
			resultMetadata.setCreatedBy(userId);
		} else {
			resultMetadata.fromBaseMetadata(targetMetadata);

			resultMetadata.setFolder(sourceMetadata.isFolder());
			resultMetadata.setSize(sourceMetadata.getSize());
		}
		resultMetadata.setName(sourceMetadata.getRealName());
		resultMetadata.setOriginName(sourceMetadata.getRealOriginalName());

		resultMetadata.setParentId(targetParentId);
		resultMetadata.setModifiedAt(DateUtils.nowUTCTimestamp());
		resultMetadata.setModifiedBy(userId);
		if (sourceMetadata instanceof Metadata) {
			resultMetadata.setBackupCount(((Metadata) sourceMetadata)
					.getBackupCount());
		}
		resultMetadata.setDeviceUid("");
		resultMetadata.setFullSourcePath("");
		resultMetadata.setNote(sourceMetadata.getNote());
		resultMetadata.setParams(sourceMetadata.getParams());
		resultMetadata.setIconBlockId(sourceMetadata.getIconBlockId());
		resultMetadata.setEncrypted(sourceMetadata.isEncrypted());
		resultMetadata.setBlockId(sourceMetadata.getBlockId());

		if (!isMove) {
			resultMetadata.setType(MetadataType.NORMAL.toString());
			resultMetadata.setSharedRootId(sharedRootId);
			resultMetadata.setSyncRootId(syncRootId);
		} else {
			resultMetadata.setSharedRootId(sourceMetadata.getSharedRootId());
			if (!sourceMetadata.getType()
					.equals(MetadataType.NORMAL.toString())) {
				resultMetadata.setSyncRootId(sourceMetadata.getSyncRootId());
			}
			resultMetadata.setType(sourceMetadata.getType());
			resultMetadata.setDeviceUid(sourceMetadata.getDeviceUid());
			resultMetadata
					.setFullSourcePath(sourceMetadata.getFullSourcePath());
			resultMetadata.setId(sourceMetadata.getId());
		}
		resultMetadata.setBlockId(sourceMetadata.getBlockId());
		resultMetadata.setOwnerId(targetOwnerId);
		resultMetadata.setHeight(sourceMetadata.getHeight());
		resultMetadata.setWidth(sourceMetadata.getWidth());
		resultMetadata.setRefId(sourceMetadata.getRefId());
		resultMetadata.setRefToOwnerId(sourceMetadata.getRefToOwnerId());
		return resultMetadata;
	}

	@Override
	@Transactional
	public void setProfilePoperty(ProfilePropertyRequest profilePropertyRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(profilePropertyRequest,
				true, true);
		BaseMetadata folder = null;
		if (pathInfo.isUnderMyBackupData()) {
			String lowerCaseOwnerPath = StringUtils.trimFilePathRoot(
					pathInfo.getFullOwnerPath()).toLowerCase();
			if (lowerCaseOwnerPath.equals(CommonFolders.MY_BACKUP_DATA
					.toString())) {
				folder = backupMetadataDao.getBackupMetadataByPath(
						pathInfo.getOwnerId(), "", "", true);
			} else {
				folder = backupMetadataDao.getBackupMetadataByPath(
						pathInfo.generateBackupFullOwnerPath(), "", "", true);
			}
		} else {
			folder = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
					"", "");
		}

		if (folder == null) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		ProfileProperty profileProperty = new ProfileProperty();
		profileProperty.setProfileId(0);
		profileProperty.setUserId(pathInfo.getOwnerId());
		profileProperty.setDeviceUid(profilePropertyRequest.getDeviceUid());
		profileProperty.setMetadataIndexId(folder.getId());
		profileProperty.setPropertyName(profilePropertyRequest.getName());
		profileProperty.setPropertyValue(profilePropertyRequest.getValue());
		profilePropertyDao.replaceProfileProperty(profileProperty);

		// activity history
		String sharedUpperPath = metadataFactory
				.getSharedUpperPath(profilePropertyRequest.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", folder.getId(),
				profilePropertyRequest.getUserId(), DateUtils.nowUTCDateTime(),
				ChangeReason.UPDATE_PROFILE_PROPERTY.toString(),
				profilePropertyRequest.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());

		// sync
		if (!syncRelationService.isCaller()) {
			syncRelationService.sync(profilePropertyRequest, folder,
					SystemEvent.SET_PROFILE_PROPERTY);
		}
	}

	@Override
	public List<ProfileProperty> getProfilePoperty(
			ProfilePropertyRequest profilePropertyRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(profilePropertyRequest,
				true, true);
		BaseMetadata folder = null;
		if (pathInfo.isUnderMyBackupData()) {
			String lowerCaseOwnerPath = StringUtils.trimFilePathRoot(
					pathInfo.getFullOwnerPath()).toLowerCase();
			if (lowerCaseOwnerPath.equals(CommonFolders.MY_BACKUP_DATA
					.toString())) {
				folder = backupMetadataDao.getBackupMetadataByPath(
						pathInfo.getOwnerId(), "", "", true);
			} else {
				folder = backupMetadataDao.getBackupMetadataByPath(
						pathInfo.generateBackupFullOwnerPath(), "", "", true);
			}
		} else {
			folder = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
					"", "");
		}

		if (folder == null) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		List<ProfileProperty> properties = profilePropertyDao
				.getProfileProperties(pathInfo.getOwnerId(), folder.getId());
		String propertyName = profilePropertyRequest.getName();
		if (!StringUtils.isNullOrEmpty(propertyName)) {
			List<ProfileProperty> newProperties = new ArrayList<ProfileProperty>();
			for (ProfileProperty property : properties) {
				if (property.getPropertyName().equals(propertyName)) {
					newProperties.add(property);
				}
			}
			properties = newProperties;
		}

		return properties;
	}

	@Override
	@Transactional
	public void deleteProfilePoperty(
			ProfilePropertyRequest profilePropertyRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(profilePropertyRequest,
				true, true);
		BaseMetadata folder = pathInfo.isUnderMyBackupData() ? backupMetadataDao
				.getBackupMetadataByPath(
						pathInfo.generateBackupFullOwnerPath(), "", "", true)
				: metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
						"", "");

		if (folder == null) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		profilePropertyDao.deleteProfileAndProperties(pathInfo.getOwnerId(),
				profilePropertyRequest.getDeviceUid(), folder.getId());

		// sync
		if (!syncRelationService.isCaller()) {
			syncRelationService.sync(profilePropertyRequest, folder,
					SystemEvent.DELETE_PROFILE_PROPERTY);
		}
	}

	@Override
	public GetBulkInfoResponse getBulkInfo(GetBulkInfoRequest getBulkInfoRequest)
			throws MetadataException {

		List<FolderResponse> folders = new ArrayList<FolderResponse>();
		List<FileResponse> files = new ArrayList<FileResponse>();
		GetBulkInfoResponse getBulkInfoResponse = new GetBulkInfoResponse();
		getBulkInfoResponse.setFolders(folders);
		getBulkInfoResponse.setFiles(files);

		HashMap<String, String> mapDeviceName = new HashMap<String, String>();
		GetAllDeviceRequest request = new GetAllDeviceRequest();
		request.setUserId(getBulkInfoRequest.getUserId());
		request.setToken(getBulkInfoRequest.getToken());
		List<GetDeviceResponse> devices = deviceService.getAllDevice(request)
				.getDevices();
		for (GetDeviceResponse device : devices) {
			mapDeviceName.put(device.getDeviceUid(), device.getName());
		}

		for (int i = 0; i < getBulkInfoRequest.size(); i++) {
			BulkInfoItem bulkFileInfoItem = getBulkInfoRequest.get(i);
			Metadata metadata = metadataDao.getMetadata(bulkFileInfoItem
					.getId());

			if (metadata == null)
				continue;

			if (!metadata.getSharedRootId().equals("")) {
				try {
					Boolean canAccess = collaborateService
							.canAccessCollaborate(metadata.getSharedRootId(),
									metadata.getOwnerId(),
									getBulkInfoRequest.getUserId(), "");
					if (!canAccess)
						continue;
				} catch (Exception ex) {
					continue;
				}
			}

			Map<String, Object> appendOtherParams = new HashMap<String, Object>();
			appendOtherParams.put("id", bulkFileInfoItem.getId());
			appendOtherParams.put("score", bulkFileInfoItem.getScore());
			appendOtherParams.put("highlights",
					bulkFileInfoItem.getHighlights());

			if (metadata.isFolder()) {
				folders.add(metadataFactory.toFolderResponse(metadata, true,
						appendOtherParams));
			} else {
				FileResponse response = metadataFactory.toFileResponse(
						metadata, true, appendOtherParams);
				response.setDeviceName(mapDeviceName.get(metadata
						.getDeviceUid()));
				files.add(response);
			}
		}

		return getBulkInfoResponse;
	}

	@Override
	@Transactional
	public void renameLinkFolder(RenameFolderRequest renameFolderRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(renameFolderRequest,
				true, true);
		String oldFullPath = pathInfo.getFullSourcePath();

		if (StringUtils.isNullOrEmpty(oldFullPath)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		oldFullPath = oldFullPath.trim();
		if (oldFullPath.endsWith("/")) {
			oldFullPath = oldFullPath.substring(0, oldFullPath.length() - 1);
		}

		// {userId}/My Device Sync Folders/{deviceUniqueId}/{path...}
		if (oldFullPath.split("/").length < 4) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		String newFullPath = FilenameUtils.getPath(oldFullPath)
				+ renameFolderRequest.getNewName();

		// UPDATE metadata & backup

		metadataDao.renameLinkFolder(oldFullPath, newFullPath);
		backupMetadataDao.renameLinkFolder(oldFullPath, newFullPath);

		// UPDATE sync relation

		// My Device Sync Folders/{deviceUniqueId}/{path...}
		String oldSourcePath = oldFullPath
				.substring(oldFullPath.indexOf("/") + 1);
		String newSourcePath = newFullPath
				.substring(newFullPath.indexOf("/") + 1);
		;
		syncRelationDao.renameLinkFolder(pathInfo.getOwnerId(), oldSourcePath,
				newSourcePath);
	}

	@Override
	@Transactional
	public CreateCollaborateResponse transformSyncToShared(
			CreateCollaborateRequest createCollaborateRequest)
			throws MetadataException {
		// sync -> normal
		syncRelationService.transformSyncToNormal(createCollaborateRequest);

		// normal -> shared
		return collaborateService.transformToShared(createCollaborateRequest);
	}

	@Override
	@Transactional
	public void transformSharedToSync(SharedToSyncRequest sharedToSyncRequest)
			throws MetadataException {
		CancelCollaborateRequest cancelCollaborateRequest = new CancelCollaborateRequest();
		cancelCollaborateRequest.fromRequestBase(sharedToSyncRequest);
		cancelCollaborateRequest.setReturnToCreator(sharedToSyncRequest
				.isReturnToCreator());

		PathInfo sourcePathInfo = pathFactory.parsePathInfo(
				sharedToSyncRequest, true, true);
		PathInfo targetPathInfo = pathFactory
				.parsePathInfo(sharedToSyncRequest
						.parseDestinationToSyncRelationPathRequest(), true,
						true);

		if (!sourcePathInfo.isUnderMyDeviceSyncFolders()) {
			cancelCollaborateRequest.setPath(sharedToSyncRequest.getPath());
			cancelCollaborateRequest.setSourcePath("");
			cancelCollaborateRequest.setOwnerId(sourcePathInfo.getOwnerId());
			cancelCollaborateRequest.setSharedRootId(sharedToSyncRequest
					.getSharedRootId());
			collaborateService.transformToNormal(cancelCollaborateRequest);
		}

		if (!targetPathInfo.isUnderMyDeviceSyncFolders()) {
			cancelCollaborateRequest.setPath(sharedToSyncRequest
					.getDestinationPath());
			cancelCollaborateRequest.setSourcePath("");
			cancelCollaborateRequest.setOwnerId(targetPathInfo.getOwnerId());
			cancelCollaborateRequest.setSharedRootId(sharedToSyncRequest
					.getDestinationSharedRootId());
			collaborateService.transformToNormal(cancelCollaborateRequest);
		}

		syncRelationService.transformNormalToSync(sharedToSyncRequest);
	}

	public GetBulkFileInfoResponse ambiguousSearch(
			GetSearchResultRequest getSearchResultRequest)
			throws MetadataException {
		List<FolderResponse> folders = new ArrayList<FolderResponse>();
		List<FileResponse> files = new ArrayList<FileResponse>();
		String keyword = StringUtils.escapeSqlLikeString(getSearchResultRequest
				.getQuery());
		List<BaseMetadata> metadatas = metadataDao
				.getMetadatasBySearchingNameAndNote(
						getSearchResultRequest.getUserId(), keyword);
		if (getSearchResultRequest.isIncludeBackup()) {
			List<? extends BaseMetadata> backupMetadata = backupMetadataDao
					.getBackupMetadatasBySearchingNameAndNote(
							getSearchResultRequest.getUserId(), keyword);
			metadatas.addAll(backupMetadata);
		}
		for (BaseMetadata item : metadatas) {
			if (item == null
					|| (item.isFolder() && item.getName().equals(
							getSearchResultRequest.getUserId())))
				continue;
			// check can access shared folder
			// TODO

			// erase folder/file under my communication data
			Map<String, Object> appendOtherParams = new HashMap<String, Object>();

			appendOtherParams.put("score", 100);
			// add highlights according to keyword
			String highlights = item.getNote().contains(
					getSearchResultRequest.getQuery()) ? StringUtils
					.escapeHTML(item.getNote()) : StringUtils.escapeHTML(item
					.getName());
			highlights = highlights.replace(getSearchResultRequest.getQuery(),
					"<em>" + getSearchResultRequest.getQuery() + "</em>");
			appendOtherParams.put("highlights", highlights);
			if (item.isFolder()) {
				FolderResponse folderResponse = metadataFactory
						.toFolderResponse(item, true, appendOtherParams);

				if (checkResponse(folderResponse.getPath(),
						getSearchResultRequest)) {
					folders.add(folderResponse);
				}
			} else {
				FileResponse fileResponse = metadataFactory.toFileResponse(
						item, true, appendOtherParams);

				if (checkResponse(fileResponse.getPath(),
						getSearchResultRequest)) {
					files.add(fileResponse);
				}
			}
		}
		// sort(by path) and paging
		SortType sortByType = SortType.PATH;
		metadataFactory.sortFolderResponses(folders, sortByType);
		metadataFactory.sortFileResponses(files, sortByType);
		int totalFolderSize = folders.size();
		folders = metadataFactory.pagingFolderResponses(folders,
				getSearchResultRequest.getPageNumber(),
				getSearchResultRequest.getItemsPerPage(), 0);
		files = metadataFactory.pagingFileResponses(files,
				getSearchResultRequest.getPageNumber(),
				getSearchResultRequest.getItemsPerPage(), totalFolderSize);
		// result
		int totalPages = MathUtils.ceil((folders.size() + files.size()),
				getSearchResultRequest.getItemsPerPage(), 1);
		return new GetBulkFileInfoResponse(totalPages, folders, files);
	}

	private boolean checkResponse(String path,
			GetSearchResultRequest getSearchResultRequest) {
		String[] pathFragmenets = path.toLowerCase().split("/");
		String parentPath = FilenameUtils.getPath(path);
		if (!pathFragmenets[0].equals("my communication data")
				&& (StringUtils.isNullOrEmpty(getSearchResultRequest.getPath())
						|| parentPath.toLowerCase().startsWith(
								getSearchResultRequest.getPath().concat("/")
										.toLowerCase()) || parentPath
						.toLowerCase().startsWith(
								"My Backup Data/"
										.concat(getSearchResultRequest
												.getPath()).concat("/")
										.toLowerCase()))) {
			return true;
		}
		return false;
	}

	@Override
	public void cancelProcess(CancelProcessRequest cancelProcessRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(cancelProcessRequest,
				true, true);
		BaseMetadata metadata = cancelProcessRequest.getFolder() ? metadataFactory
				.getFolderByPathInfo(pathInfo) : metadataFactory
				.getFileByPathInfo(pathInfo);

		FileProcessingStatus fileProcessingStatus = fileProcessingStatusDao
				.getFileProcessingStatus(metadata.getId());
		if (fileProcessingStatus == null) {
			throw new MetadataException(HttpStatus.PROCESS_NOT_EXIST);
		}
		if (fileProcessingStatus.getProcessingStatus() == ProcessingStatus.LOCKING) {
			throw new MetadataException(HttpStatus.PROCESS_IS_LOCKED);
		}
		if (fileProcessingStatus.getProcessingStatus() != ProcessingStatus.PROCESSING
				&& fileProcessingStatus.getProcessingStatus() != ProcessingStatus.WAITING) {
			throw new MetadataException(HttpStatus.PROCESS_NOT_EXIST);
		}

		fileProcessingStatus = new FileProcessingStatus(metadata.getId(),
				ProcessingStatus.CANCEL, 0, "",
				fileProcessingStatus.getProcessAction());
		fileProcessingStatusService
				.updateFileProcessingStatus(fileProcessingStatus);

		// send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata
				.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.UPDATE_PROCESSING_STATUS.toString(),
				metadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.UPDATE_FILE_PROCESSING_STATUS,
				pathInfo);
		// sync
		if (!syncRelationService.isCaller()) {
			syncRelationService.sync(cancelProcessRequest, metadata,
					SystemEvent.UPDATE_FILE_PROCESSING_STATUS);
		}
	}

	@Override
	@Transactional
	public void setItemStyle(SetItemStyleRequest setItemStyleRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(setItemStyleRequest,
				setItemStyleRequest.isFolder(), true);
		BaseMetadata metadata = setItemStyleRequest.isFolder() ? metadataFactory
				.getFolderByPathInfo(pathInfo) : metadataFactory
				.getFileByPathInfo(pathInfo);
		if (setItemStyleRequest.getPath().isEmpty()) {
			metadataDao.updateGlobalItemStyle(pathInfo.getOwnerId(),
					setItemStyleRequest.getItemTextColor(),
					setItemStyleRequest.getItemBgColor(),
					setItemStyleRequest.isItemTextBold(),
					setItemStyleRequest.isItemTextItalic(),
					setItemStyleRequest.isItemTextUnderline());
		}
		metadataDao.updateItemStyle(metadata.getId(),
				setItemStyleRequest.getItemTextColor(),
				setItemStyleRequest.getItemBgColor(),
				setItemStyleRequest.isItemTextBold(),
				setItemStyleRequest.isItemTextItalic(),
				setItemStyleRequest.isItemTextUnderline());

		// send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata
				.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.SET_ITEM_STYLE.toString(),
				metadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.SET_ITEM_STYLE, pathInfo);
	}

	@Override
	public void setSubItemsStyle(SetSubItemsStyleRequest setSubItemsStyleRequest)
			throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(setSubItemsStyleRequest,
				true, true);
		BaseMetadata metadata = metadataFactory.getFolderByPathInfo(pathInfo);
		metadataDao.updateSubItemsStyle(metadata.getId(),
				setSubItemsStyleRequest.getItemTextColor(),
				setSubItemsStyleRequest.getItemBgColor(),
				setSubItemsStyleRequest.isItemTextBold(),
				setSubItemsStyleRequest.isItemTextItalic(),
				setSubItemsStyleRequest.isItemTextUnderline());

		// send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(metadata
				.getSharedRootId());
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				sharedUpperPath, "", metadata.getId(), pathInfo.getUserId(),
				DateUtils.nowUTCDateTime(),
				ChangeReason.SET_SUB_ITEMS_STYLE.toString(),
				metadata.getSharedRootId(),
				ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.SET_SUB_ITEMS_STYLE, pathInfo);
	}
}
