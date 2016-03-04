package controller.impl;

import java.util.List;

import model.BulkRequestBase;
import model.CountResult;
import model.PathRequestBase;
import model.PriorityInfo;
import model.ProfileProperty;
import model.RequestBase;
import model.request.AddGlobalIconRequest;
import model.request.BulkActionRequest;
import model.request.BulkActionRequestItem;
import model.request.BulkCopyRequestItem;
import model.request.BulkCreateRequestItem;
import model.request.BulkDeleteRequestItem;
import model.request.BulkMoveRequestItem;
import model.request.CancelProcessRequest;
import model.request.CopyFolderRequest;
import model.request.CountRequest;
import model.request.CreateCollaborateRequest;
import model.request.CreateGlobalIconGroupRequest;
import model.request.DeleteFolderBackupRequest;
import model.request.DeleteFolderRequest;
import model.request.DeleteGlobalIconGroupRequest;
import model.request.DeleteGlobalIconRequest;
import model.request.EncryptRequest;
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
import model.request.SyncRelationPathRequest;
import model.request.SyncRelationRequest;
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
import model.response.FolderResponse;
import model.response.GetBulkFileInfoResponse;
import model.response.GetBulkInfoResponse;
import model.response.GetFolderBackupCountResponse;
import model.response.GetFolderNoteResponse;
import model.response.GetFolderResponse;
import model.response.GetGlobalIconResponse;
import model.response.GetSubBackupCountResponse;
import model.response.LinkFoldersResponse;
import model.response.SyncRelationResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import service.CountService;
import service.FolderService;
import service.SyncRelationService;
import utils.StringUtils;
import annotation.AllowAdminAuthorization;
import annotation.Readonly;
import constants.HttpStatus;
import constants.SystemEvent;
import controller.FileContorller;
import controller.FolderController;
import exception.MetadataException;
import factory.PathFactory;

@RestController
public class FolderControllerImpl implements FolderController {

	@Autowired
	private FolderService folderService;
	@Autowired
	private FileContorller fileController;
	@Autowired
	private SyncRelationService syncRelationService;
	@Autowired
	private PathFactory pathFactory;
	@Autowired
	private CountService countService;

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/info", method = RequestMethod.POST)
	public GetFolderResponse getFolder(
			@RequestBody GetFolderRequest getFolderRequest)
			throws MetadataException {
		if (getFolderRequest.getPath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.getFolder(getFolderRequest);
	}
	
	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/detail_info", method = RequestMethod.POST)
	public FolderResponse getFolderDetail(@RequestBody PathRequestBase pathRequestBase)
			throws MetadataException {
		if (pathRequestBase.getPath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.getFolderDetail(pathRequestBase);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/create", method = RequestMethod.POST)
	public void createFolder(
			@RequestBody UpdateFolderRequest updateFolderRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFolderRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		pathFactory.checkCanCreateFolder(updateFolderRequest.getPath());
		folderService.createFolder(updateFolderRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/update", method = RequestMethod.POST)
	public void updateFolder(@RequestBody PathRequestBase updateFolderRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFolderRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.updateFolder(updateFolderRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/delete", method = RequestMethod.POST)
	public void deleteFolder(
			@RequestBody DeleteFolderRequest deleteFolderRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(deleteFolderRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.deleteFolder(deleteFolderRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/rename", method = RequestMethod.POST)
	public void renameFolder(
			@RequestBody RenameFolderRequest renameFolderRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(renameFolderRequest.getPath())
				|| StringUtils.isNullOrEmpty(renameFolderRequest.getNewName())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.renameFolder(renameFolderRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/rename_linkfolder", method = RequestMethod.POST)
	public void renameLinkFolder(
			@RequestBody RenameFolderRequest renameFolderRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(renameFolderRequest.getPath())
				|| StringUtils.isNullOrEmpty(renameFolderRequest.getNewName())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		renameFolderRequest.setOwnerId("");
		renameFolderRequest.setSharedRootId("");
		folderService.renameLinkFolder(renameFolderRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/backupcount", method = RequestMethod.POST)
	public void updateBackupCount(
			@RequestBody UpdateFolderBackupCountRequest updateFolderBackupCountRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFolderBackupCountRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.updateBackupCount(updateFolderBackupCountRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/folder/getbackupcount", method = RequestMethod.POST)
	public GetFolderBackupCountResponse getBackupCount(
			@RequestBody PathRequestBase getFolderBackupCountRequest)
			throws MetadataException {
		if (getFolderBackupCountRequest.getPath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.getBackupCount(getFolderBackupCountRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/priority", method = RequestMethod.POST)
	public void updatePriority(
			@RequestBody UpdatePriorityRequest updatePriorityRequest)
			throws MetadataException {
		if (updatePriorityRequest.getPath() == null
				|| updatePriorityRequest.getPriority() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		for (PriorityInfo priority : updatePriorityRequest.getPriority()) {
			if (StringUtils.isNullOrEmpty(priority.getName())
					|| priority.getSourcePath() == null) {
				throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
			}
		}
		folderService.updatePriority(updatePriorityRequest);
	}

	@Deprecated
	@Override
	@RequestMapping(value = "/api/v1/folder/unlink", method = RequestMethod.POST)
	public void unlinkFolder(@RequestBody PathRequestBase unlinkFolderRequest)
			throws MetadataException {

		if (StringUtils.isNullOrEmpty(unlinkFolderRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.unlinkFolder(unlinkFolderRequest);
	}

	@Deprecated
	@Override
	@RequestMapping(value = "/api/v1/folder/link", method = RequestMethod.POST)
	public void linkFolder(@RequestBody PathRequestBase linkFolderRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(linkFolderRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.linkFolder(linkFolderRequest);
	}

	@Deprecated
	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/folder/linkfolders", method = RequestMethod.POST)
	public LinkFoldersResponse linkFolders(
			@RequestBody LinkFoldersRequest linkFolderRequest)
			throws MetadataException {
		if (linkFolderRequest.getPath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.linkFolders(linkFolderRequest);
	}

	@Deprecated
	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/folder/unlinkfolders", method = RequestMethod.POST)
	public LinkFoldersResponse unlinkFolders(
			@RequestBody RequestBase linkFolderRequest)
			throws MetadataException {
		return folderService.unlinkFolders(linkFolderRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/note", method = RequestMethod.POST)
	public void updateFolderNote(
			@RequestBody UpdateFolderNoteRequest updateFolderNoteRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFolderNoteRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.updateFolderNote(updateFolderNoteRequest);
	}

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/get_note", method = RequestMethod.POST)
	public GetFolderNoteResponse getFolderNote(
			@RequestBody PathRequestBase getFolderNoteRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(getFolderNoteRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.getFolderNote(getFolderNoteRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/delete_backup", method = RequestMethod.POST)
	public void deleteFolderBackup(
			@RequestBody DeleteFolderBackupRequest deleteFolderBackupRequest)
			throws MetadataException {
		if (null == deleteFolderBackupRequest.getPath()) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.deleteFolderBackup(deleteFolderBackupRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/folder/getsubbackupcount", method = RequestMethod.POST)
	public List<GetSubBackupCountResponse> getSubBackupCount(
			@RequestBody PathRequestBase getSubBackupCountRequest)
			throws MetadataException {
		if (getSubBackupCountRequest.getPath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.getSubBackupCount(getSubBackupCountRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/zip", method = RequestMethod.POST)
	public void zip(@RequestBody ZipRequest zipRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(zipRequest.getTargetPath())
				|| zipRequest.getZipSource() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		for (ZipSource zipSource : zipRequest.getZipSource()) {
			if (StringUtils.isNullOrEmpty(zipSource.getPath())
					|| zipSource.getSourcePath() == null) {
				throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
			}
		}
		folderService.zip(zipRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/unzip", method = RequestMethod.POST)
	public void unzip(@RequestBody UnzipRequest zipRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(zipRequest.getTargetPath())
				|| StringUtils.isNullOrEmpty(zipRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.unzip(zipRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/move", method = RequestMethod.POST)
	public void moveFolder(@RequestBody MoveFolderRequest moveFolderRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(moveFolderRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.moveFolder(moveFolderRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/copy", method = RequestMethod.POST)
	public void copyFolder(@RequestBody CopyFolderRequest copyFolderRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(copyFolderRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.copyFolder(copyFolderRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/add_global_icon", method = RequestMethod.POST)
	public void addGlobalIcon(
			@RequestBody AddGlobalIconRequest addGlobalIconRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(addGlobalIconRequest.getBlockId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.addGlobalIcon(addGlobalIconRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/delete_global_icon", method = RequestMethod.POST)
	public void deleteGlobalIcon(
			@RequestBody DeleteGlobalIconRequest deleteGlobalIconRequest)
			throws MetadataException {
		if (deleteGlobalIconRequest.getBlockIds() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.deleteGlobalIcon(deleteGlobalIconRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/clean_unuse_global_icon", method = RequestMethod.POST)
	public void cleanUnuseGlobalIcon(
			@RequestBody RequestBase cleanUnuseGlobalIconRequest)
			throws MetadataException {
		folderService.cleanUnuseGlobalIcon(cleanUnuseGlobalIconRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/reset_global_icon", method = RequestMethod.POST)
	public void resetGlobalIcon(
			@RequestBody ResetGlobalIconRequest reseteGlobalIconRequest)
			throws MetadataException {
		folderService.resetGlobalIcon(reseteGlobalIconRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/folder/get_global_icon", method = RequestMethod.POST)
	public GetGlobalIconResponse getGlobalIcon(
			@RequestBody GetGlobalIconRequest getGlobalIconRequest)
			throws MetadataException {
		if (getGlobalIconRequest.getFolder() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.getGlobalIcon(getGlobalIconRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/set_icon", method = RequestMethod.POST)
	public void updateIcon(@RequestBody UpdateIconRequest updateIconRequest)
			throws MetadataException {
		folderService.updateIcon(updateIconRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/move_global_icon", method = RequestMethod.POST)
	public void moveGlobalIcon(
			@RequestBody MoveGlobalIconRequest moveGlobalIconRequest)
			throws MetadataException {
		folderService.moveGlobalIcon(moveGlobalIconRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/create_global_icon_group", method = RequestMethod.POST)
	public void createGlobalIconGroup(
			@RequestBody CreateGlobalIconGroupRequest createGlobalIconGroupRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(createGlobalIconGroupRequest.getName())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.createGlobalIconGroup(createGlobalIconGroupRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/delete_global_icon_group", method = RequestMethod.POST)
	public void deleteGlobalIconGroup(
			@RequestBody DeleteGlobalIconGroupRequest deleteGlobalIconGroupRequest)
			throws MetadataException {
		if (deleteGlobalIconGroupRequest.getFolder() == null
				|| deleteGlobalIconGroupRequest.getName() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.deleteGlobalIconGroup(deleteGlobalIconGroupRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/rename_global_icon_group", method = RequestMethod.POST)
	public void renameGlobalIconGroup(
			@RequestBody RenameGlobalIconGroupRequest renameGlobalIconGroupRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(renameGlobalIconGroupRequest.getName())
				|| StringUtils.isNullOrEmpty(renameGlobalIconGroupRequest
						.getNewName())) {
			throw new MetadataException(HttpStatus.VALUE_CANT_BE_NULL);
		}
		folderService.renameGlobalIconGroup(renameGlobalIconGroupRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/encrypt", method = RequestMethod.POST)
	public void encryptFolder(@RequestBody EncryptRequest encryptRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(encryptRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.encryptFolder(encryptRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/after_encrypt", method = RequestMethod.POST)
	public void afterEncrypt(@RequestBody UpdateFileRequest updateFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFileRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.afterEncrypt(updateFileRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/after_decrypt", method = RequestMethod.POST)
	public void afterDecrypt(
			@RequestBody BulkActionRequest<BulkCreateRequestItem> bulkActionRequest)
			throws MetadataException {
		folderService.afterDecrypt(bulkActionRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/property", method = RequestMethod.POST)
	public void setProfilePoperty(
			@RequestBody ProfilePropertyRequest profilePropertyRequest)
			throws MetadataException {
		if (profilePropertyRequest.getPath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		} else if (profilePropertyRequest.getDeviceUid() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		} else if (StringUtils.isNullOrEmpty(profilePropertyRequest.getName())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		} else if (profilePropertyRequest.getValue() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.setProfilePoperty(profilePropertyRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/folder/get_property", method = RequestMethod.POST)
	public List<ProfileProperty> getProfilePoperty(
			@RequestBody ProfilePropertyRequest profilePropertyRequest)
			throws MetadataException {
		if (profilePropertyRequest.getPath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.getProfilePoperty(profilePropertyRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/delete_property", method = RequestMethod.POST)
	public void deleteProfilePoperty(
			@RequestBody ProfilePropertyRequest profilePropertyRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(profilePropertyRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.deleteProfilePoperty(profilePropertyRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/add_sync_relation", method = RequestMethod.POST)
	public void addSyncRelation(
			@RequestBody SyncRelationRequest syncRelationRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(syncRelationRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (StringUtils.isNullOrEmpty(syncRelationRequest.getDestinationPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		try {
			syncRelationService.addSyncRelation(syncRelationRequest);
		} catch (IllegalArgumentException e) {
			throw new MetadataException(HttpStatus.INVALID_SYNC_RELATION_ACTION);
		}
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/folder/query_sync_relation", method = RequestMethod.POST)
	public List<SyncRelationResponse> querySyncRelation(
			@RequestBody SyncRelationPathRequest syncRelationPathRequest)
			throws MetadataException {
		try {
			return syncRelationService
					.querySyncRelation(syncRelationPathRequest);
		} catch (IllegalArgumentException e) {
			throw new MetadataException(HttpStatus.INVALID_SYNC_RELATION_ACTION);
		}
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/update_sync_relation", method = RequestMethod.POST)
	public void updateSyncRelation(
			@RequestBody SyncRelationRequest syncRelationRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(syncRelationRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (StringUtils.isNullOrEmpty(syncRelationRequest.getDestinationPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		try {
			syncRelationService.updateSyncRelation(syncRelationRequest);
		} catch (IllegalArgumentException e) {
			throw new MetadataException(HttpStatus.INVALID_SYNC_RELATION_ACTION);
		}
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/delete_sync_relation", method = RequestMethod.POST)
	public void deleteSyncRelation(
			@RequestBody SyncRelationRequest syncRelationRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(syncRelationRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (StringUtils.isNullOrEmpty(syncRelationRequest.getDestinationPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		try {
			syncRelationService.deleteSyncRelation(syncRelationRequest);
		} catch (IllegalArgumentException e) {
			throw new MetadataException(HttpStatus.INVALID_SYNC_RELATION_ACTION);
		}
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/normal_to_sync", method = RequestMethod.POST)
	public void transformNormalToSync(
			@RequestBody SyncRelationRequest syncRelationRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(syncRelationRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (StringUtils.isNullOrEmpty(syncRelationRequest.getDestinationPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		try {
			syncRelationService.transformNormalToSync(syncRelationRequest);
		} catch (IllegalArgumentException e) {
			throw new MetadataException(HttpStatus.INVALID_SYNC_RELATION_ACTION);
		}
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/sync_to_normal", method = RequestMethod.POST)
	public void transformSyncToNormal(@RequestBody PathRequestBase pathRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(pathRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		syncRelationService.transformSyncToNormal(pathRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/shared_to_sync", method = RequestMethod.POST)
	public void transformSharedToSync(
			@RequestBody SharedToSyncRequest sharedToSyncRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(sharedToSyncRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (StringUtils.isNullOrEmpty(sharedToSyncRequest.getDestinationPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		try {
			folderService.transformSharedToSync(sharedToSyncRequest);
		} catch (IllegalArgumentException e) {
			throw new MetadataException(HttpStatus.INVALID_SYNC_RELATION_ACTION);
		}
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/sync_to_shared", method = RequestMethod.POST)
	public CreateCollaborateResponse transformSyncToShared(
			@RequestBody CreateCollaborateRequest createCollaborateRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(createCollaborateRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.transformSyncToShared(createCollaborateRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/folder/bulk_copy", method = RequestMethod.POST)
	public void bulkCopy(@RequestBody BulkActionRequest<BulkCopyRequestItem> bulkActionRequest)
			throws MetadataException {
		bulkExecute(SystemEvent.BULK_COPY, bulkActionRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/bulk_move", method = RequestMethod.POST)
	public void bulkMove(@RequestBody BulkActionRequest<BulkMoveRequestItem> bulkActionRequest)
			throws MetadataException {
		bulkExecute(SystemEvent.BULK_MOVE, bulkActionRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/bulk_delete", method = RequestMethod.POST)
	public void bulkDelete(@RequestBody BulkActionRequest<BulkDeleteRequestItem> bulkActionRequest)
			throws MetadataException {
		bulkExecute(SystemEvent.BULK_DELETE, bulkActionRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/bulk_create", method = RequestMethod.POST)
	public void bulkCreate(@RequestBody BulkActionRequest<BulkCreateRequestItem> bulkActionRequest)
			throws MetadataException {
		bulkExecute(SystemEvent.BULK_CREATE, bulkActionRequest);
	}

	@Override
	@ResponseBody
	@RequestMapping(value = "/api/v1/folder/sub_count", method = RequestMethod.POST)
	public CountResult getSubCount(@RequestBody CountRequest request)
			throws MetadataException {
		return countService.getSubCount(request);
	}

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/folder/bulk_info", method = RequestMethod.POST)
	public GetBulkInfoResponse getBulkInfo(
			@RequestBody GetBulkInfoRequest getBulkInfoRequest)
			throws MetadataException {
		if (getBulkInfoRequest == null || getBulkInfoRequest.size() <= 0) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.getBulkInfo(getBulkInfoRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/bulk_property", method = RequestMethod.POST)
	public void bulkSetProfilePoperty(@RequestBody BulkRequestBase<ProfilePropertyRequest> bulkProfilePropertyRequest) throws MetadataException {
		if (bulkProfilePropertyRequest == null || bulkProfilePropertyRequest.size() <= 0) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		for (ProfilePropertyRequest profilePropertyRequest : bulkProfilePropertyRequest) {
			folderService.setProfilePoperty(profilePropertyRequest);
		}
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/folder/ambiguous_search", method = RequestMethod.POST)
	public GetBulkFileInfoResponse ambiguousSearch(
			@RequestBody GetSearchResultRequest getSearchResultRequest)
			throws MetadataException {
		if (getSearchResultRequest == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return folderService.ambiguousSearch(getSearchResultRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/cancel_process", method = RequestMethod.POST)
	public void cancelProcess(
			@RequestBody CancelProcessRequest cancelProcessRequest)
			throws MetadataException {
		if (cancelProcessRequest.getFolder() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.cancelProcess(cancelProcessRequest);
	}
	
	
	/**
	 * Bulk execute a series of actions in a request.
	 * 
	 * @param systemEvent
	 *            must be BULK_COPY, BULK_MOVE, BULK_DELETE, or BULK_CREATE.
	 * @param bulkActionRequest
	 *            is an array that contains several items corresponding to body
	 *            of each request. Each item can be the body of folder or file.
	 * @throws MetadataException
	 *             includes HTTP status code and the corresponding item with
	 *             index that occurs the exception.
	 */
	private void bulkExecute(SystemEvent systemEvent,
			BulkActionRequest<? extends BulkActionRequestItem> bulkActionRequest) throws MetadataException {
		
		switch (systemEvent) {
			case BULK_COPY: break;
			case BULK_MOVE: break;
			case BULK_DELETE: break;
			case BULK_CREATE: break;
			default: throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		for (int i = 0; i < bulkActionRequest.size(); i++) {
			try {
				BulkActionRequestItem item = bulkActionRequest.get(i);
				item.fromRequestBase(bulkActionRequest);

				if (item.isFolder()) {
					switch (systemEvent) {
						case BULK_COPY: copyFolder(((BulkCopyRequestItem)item).toCopyFolderRequest()); break;
						case BULK_MOVE: moveFolder(((BulkMoveRequestItem)item).toMoveFolderRequest()); break;
						case BULK_DELETE: deleteFolder(((BulkDeleteRequestItem)item).toDeleteFolderRequest()); break;
						case BULK_CREATE: createFolder(((BulkCreateRequestItem)item).toCreateFolderRequest()); break;
						default: break;
					}
				} else {
					switch (systemEvent) {
						case BULK_COPY: fileController.copyFile(((BulkCopyRequestItem)item).toCopyFileRequest()); break;
						case BULK_MOVE: fileController.moveFile(((BulkMoveRequestItem)item).toMoveFileRequest()); break;
						case BULK_DELETE: fileController.deleteFile(((BulkDeleteRequestItem)item).toDeleteFileRequest()); break;
						case BULK_CREATE: fileController.createFile(((BulkCreateRequestItem)item).toCreateFileRequest()); break;
						default: break;
					}
				}
			} catch (MetadataException me) {
				throw new MetadataException(me.getHttpStatus(), bulkActionRequest.toErrorResponse(i));
			} catch (Exception ex) {
				throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR, bulkActionRequest.toErrorResponse(i));
			}
		}
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/set_item_style", method = RequestMethod.POST)
	public void setItemStyle(@RequestBody SetItemStyleRequest setItemStyleRequest)
			throws MetadataException {
		if (setItemStyleRequest.getPath() == null || setItemStyleRequest.isFolder() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (setItemStyleRequest.getItemTextColor() == null &&
			setItemStyleRequest.getItemBgColor() == null &&
			setItemStyleRequest.isItemTextBold() == null &&
			setItemStyleRequest.isItemTextItalic() == null &&
			setItemStyleRequest.isItemTextUnderline() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.setItemStyle(setItemStyleRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/folder/set_sub_items_style", method = RequestMethod.POST)
	public void setSubItemsStyle(@RequestBody SetSubItemsStyleRequest setSubItemsStyleRequest)
			throws MetadataException {
		if (setSubItemsStyleRequest.getPath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (setSubItemsStyleRequest.getItemTextColor() == null &&
			setSubItemsStyleRequest.getItemBgColor() == null &&
			setSubItemsStyleRequest.isItemTextBold() == null &&
			setSubItemsStyleRequest.isItemTextItalic() == null &&
			setSubItemsStyleRequest.isItemTextUnderline() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		folderService.setSubItemsStyle(setSubItemsStyleRequest);
	}
	
}
