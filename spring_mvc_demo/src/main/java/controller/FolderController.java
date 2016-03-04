package controller;

import java.util.List;

import model.BulkRequestBase;
import model.CountResult;
import model.PathRequestBase;
import model.ProfileProperty;
import model.RequestBase;
import model.request.AddGlobalIconRequest;
import model.request.BulkActionRequest;
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
import exception.MetadataException;

public interface FolderController {

	public GetFolderResponse getFolder(GetFolderRequest getFolderRequest)
			throws MetadataException;
	
	public FolderResponse getFolderDetail(PathRequestBase pathRequestBase)
			throws MetadataException;

	public void createFolder(UpdateFolderRequest upadateFolderRequest)
			throws MetadataException;

	public void updateFolder(PathRequestBase upadateFolderRequest)
			throws MetadataException;

	public void deleteFolder(DeleteFolderRequest deleteFolderRequest)
			throws MetadataException;

	public void renameFolder(RenameFolderRequest renameFolderRequest)
			throws MetadataException;

	public void renameLinkFolder(RenameFolderRequest renameFolderRequest)
			throws MetadataException;

	public void moveFolder(MoveFolderRequest moveFolderRequest)
			throws MetadataException;

	public void copyFolder(CopyFolderRequest copyFolderRequest)
			throws MetadataException;

	public void updateBackupCount(
			UpdateFolderBackupCountRequest updateFolderBackupCountRequest)
			throws MetadataException;

	public GetFolderBackupCountResponse getBackupCount(
			PathRequestBase getFolderBackupCountRequest)
			throws MetadataException;

	public void updatePriority(UpdatePriorityRequest updatePriorityRequest)
			throws MetadataException;

	public void unlinkFolder(PathRequestBase unlinkFolderRequest)
			throws MetadataException;

	public void linkFolder(PathRequestBase linkFolderRequest)
			throws MetadataException;

	public LinkFoldersResponse linkFolders(LinkFoldersRequest linkFolderRequest)
			throws MetadataException;

	public LinkFoldersResponse unlinkFolders(RequestBase linkFolderRequest)
			throws MetadataException;

	public void updateFolderNote(UpdateFolderNoteRequest updateFolderNoteRequest)
			throws MetadataException;

	public GetFolderNoteResponse getFolderNote(
			PathRequestBase getFolderNoteRequest) throws MetadataException;

	public void deleteFolderBackup(
			DeleteFolderBackupRequest deleteFolderBackupRequest)
			throws MetadataException;

	public List<GetSubBackupCountResponse> getSubBackupCount(
			PathRequestBase getSubBackupCountRequest) throws MetadataException;

	public void zip(ZipRequest zipRequest) throws MetadataException;

	public void unzip(UnzipRequest zipRequest) throws MetadataException;

	public void addGlobalIcon(AddGlobalIconRequest addGlobalIconRequest)
			throws MetadataException;

	public void deleteGlobalIcon(DeleteGlobalIconRequest deleteGlobalIconRequest)
			throws MetadataException;

	public void cleanUnuseGlobalIcon(RequestBase cleanUnuseGlobalIconRequest)
			throws MetadataException;

	public void resetGlobalIcon(ResetGlobalIconRequest reseteGlobalIconRequest)
			throws MetadataException;

	public GetGlobalIconResponse getGlobalIcon(
			GetGlobalIconRequest getGlobalIconRequest) throws MetadataException;

	public void moveGlobalIcon(MoveGlobalIconRequest moveGlobalIconRequest)
			throws MetadataException;

	public void createGlobalIconGroup(
			CreateGlobalIconGroupRequest createGlobalIconGroupRequest)
			throws MetadataException;

	public void deleteGlobalIconGroup(
			DeleteGlobalIconGroupRequest deleteGlobalIconGroupRequest)
			throws MetadataException;

	public void transformNormalToSync(SyncRelationRequest syncRelationRequest)
			throws MetadataException;

	public void transformSyncToNormal(PathRequestBase pathRequest)
			throws MetadataException;

	public void transformSharedToSync(SharedToSyncRequest sharedToSyncRequest)
			throws MetadataException;

	public CreateCollaborateResponse transformSyncToShared(
			CreateCollaborateRequest createCollaborateRequest)
			throws MetadataException;

	public void bulkCopy(BulkActionRequest<BulkCopyRequestItem> bulkActionRequest)
			throws MetadataException;

	public void bulkMove(BulkActionRequest<BulkMoveRequestItem> bulkActionRequest)
			throws MetadataException;

	public void bulkDelete(BulkActionRequest<BulkDeleteRequestItem> bulkActionRequest)
			throws MetadataException;

	public void bulkCreate(BulkActionRequest<BulkCreateRequestItem> bulkActionRequest)
			throws MetadataException;

	public GetBulkInfoResponse getBulkInfo(
			GetBulkInfoRequest getBulkFileInfoRequest) throws MetadataException;

	public void bulkSetProfilePoperty(BulkRequestBase<ProfilePropertyRequest> bulkProfilePropertyRequest) throws MetadataException;

	public GetBulkFileInfoResponse ambiguousSearch(
			GetSearchResultRequest getSearchResultRequest)
			throws MetadataException;

	public void cancelProcess(CancelProcessRequest cancelProcessRequest)
			throws MetadataException;

	CountResult getSubCount(CountRequest request) throws MetadataException;

	void deleteSyncRelation(SyncRelationRequest syncRelationRequest)
			throws MetadataException;

	void updateSyncRelation(SyncRelationRequest syncRelationRequest)
			throws MetadataException;

	List<SyncRelationResponse> querySyncRelation(
			SyncRelationPathRequest syncRelationPathRequest)
			throws MetadataException;

	void addSyncRelation(SyncRelationRequest syncRelationRequest)
			throws MetadataException;

	void deleteProfilePoperty(ProfilePropertyRequest profilePropertyRequest)
			throws MetadataException;

	List<ProfileProperty> getProfilePoperty(
			ProfilePropertyRequest profilePropertyRequest)
			throws MetadataException;

	void setProfilePoperty(ProfilePropertyRequest profilePropertyRequest)
			throws MetadataException;

	public void afterEncrypt(UpdateFileRequest updateFileRequest)
			throws MetadataException;

	public void afterDecrypt(
			BulkActionRequest<BulkCreateRequestItem> bulkActionRequest)
			throws MetadataException;

	public void encryptFolder(EncryptRequest encryptRequest) throws MetadataException;

	void renameGlobalIconGroup(
			RenameGlobalIconGroupRequest renameGlobalIconGroupRequest)
			throws MetadataException;

	void updateIcon(UpdateIconRequest updateIconRequest)
			throws MetadataException;
	
	public void setItemStyle(SetItemStyleRequest setItemStyleRequest)
			throws MetadataException;
	
	public void setSubItemsStyle(SetSubItemsStyleRequest setSubItemsStyleRequest)
			throws MetadataException;
	
}
