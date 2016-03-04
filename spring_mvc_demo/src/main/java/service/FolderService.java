package service;

import java.util.List;

import model.PathInfo;
import model.PathRequestBase;
import model.ProfileProperty;
import model.RequestBase;
import model.request.AddGlobalIconRequest;
import model.request.BulkActionRequest;
import model.request.BulkCreateRequestItem;
import model.request.CancelProcessRequest;
import model.request.CopyFolderRequest;
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
import model.request.UnzipRequest;
import model.request.UpdateFileRequest;
import model.request.UpdateFolderBackupCountRequest;
import model.request.UpdateFolderNoteRequest;
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
import exception.MetadataException;

public interface FolderService {

	public GetFolderResponse getFolder(GetFolderRequest getFolderRequest) throws MetadataException;
	
	public FolderResponse getFolderDetail(PathRequestBase pathRequestBase) throws MetadataException;

	public Boolean createFolder(PathRequestBase upadateFolderRequest) throws MetadataException;

	public Boolean updateFolder(PathRequestBase upadateFolderRequest) throws MetadataException;

	public void deleteFolder(String folderId, String userId, boolean extract, boolean isUnderMyBackupData) throws MetadataException;
			
	public void deleteFolder(DeleteFolderRequest deleteFolderRequest) throws MetadataException;
	
	public void deleteFolderForSuccessValidate(DeleteFolderRequest deleteFolderRequest, PathInfo pathInfo);

	public void renameFolder(RenameFolderRequest renameFolderRequest) throws MetadataException;

	public void renameLinkFolder(RenameFolderRequest renameFolderRequest) throws MetadataException;
	
	public void moveFolder(MoveFolderRequest moveFolderRequest) throws MetadataException;

	public void copyFolder(CopyFolderRequest copyFolderRequest) throws MetadataException;

	public void updateBackupCount(UpdateFolderBackupCountRequest updateFolderBackupCountRequest) throws MetadataException;

	public GetFolderBackupCountResponse getBackupCount(PathRequestBase getFolderBackupCountRequest) throws MetadataException;

	public Boolean updatePriority(UpdatePriorityRequest updatePriorityRequest) throws MetadataException;

	public void unlinkFolder(PathRequestBase unlinkFolderRequest) throws MetadataException;

	public void linkFolder(PathRequestBase linkFolderRequest) throws MetadataException;

	public LinkFoldersResponse linkFolders(LinkFoldersRequest linkFolderRequest) throws MetadataException;
	
	public LinkFoldersResponse unlinkFolders(RequestBase unlinkFoldersRequest) throws MetadataException;

	public Boolean updateFolderNote(UpdateFolderNoteRequest updateFolderNoteRequest) throws MetadataException;

	public GetFolderNoteResponse getFolderNote(PathRequestBase getFolderNoteRequest) throws MetadataException;

	public void deleteFolderBackup(DeleteFolderBackupRequest deleteFolderBackupRequest) throws MetadataException;

	public List<GetSubBackupCountResponse> getSubBackupCount(PathRequestBase getSubBackupCountRequest) throws MetadataException;

	public void zip(ZipRequest zipRequest) throws MetadataException;

	public void unzip(UnzipRequest zipRequest) throws MetadataException;

	public void addGlobalIcon(AddGlobalIconRequest addGlobalIconRequest) throws MetadataException;

	public void deleteGlobalIcon(DeleteGlobalIconRequest deleteGlobalIconRequest) throws MetadataException;
	
	public void resetGlobalIcon(ResetGlobalIconRequest resetGlobalIconRequest) throws MetadataException;
	
	public void resetGlobalIconByUser(String userId);
	
	public void cleanUnuseGlobalIcon(RequestBase cleanUnuseGlboallIconRequest) throws MetadataException;
	
	public GetGlobalIconResponse getGlobalIcon(GetGlobalIconRequest getGlobalIconRequest) throws MetadataException;

	public void moveGlobalIcon(MoveGlobalIconRequest moveGlobalIconRequest);

	public void createGlobalIconGroup(CreateGlobalIconGroupRequest createGlobalIconGroupRequest);

	public void deleteGlobalIconGroup(DeleteGlobalIconGroupRequest deleteGlobalIconGroupRequest);

	public void renameGlobalIconGroup(RenameGlobalIconGroupRequest renameGlobalIconGroupRequest);
	
	public void updateIcon(UpdateIconRequest updateIconRequest) throws MetadataException;

	public void encryptFolder(EncryptRequest encryptRequest) throws MetadataException;
	
	public void afterEncrypt(UpdateFileRequest updateFileRequest) throws MetadataException;
	
	public void afterDecrypt(BulkActionRequest<BulkCreateRequestItem> bulkActionRequest) throws MetadataException;
	
	public void setProfilePoperty(ProfilePropertyRequest profilePropertyRequest) throws MetadataException;
	
	public List<ProfileProperty> getProfilePoperty(ProfilePropertyRequest profilePropertyRequest) throws MetadataException;
	
	public void deleteProfilePoperty(ProfilePropertyRequest profilePropertyRequest) throws MetadataException;
	
	public GetBulkInfoResponse getBulkInfo(GetBulkInfoRequest getBulkInfoRequest) throws MetadataException;
	
	public CreateCollaborateResponse transformSyncToShared(CreateCollaborateRequest CreateCollaborateRequest) throws MetadataException;
	
	public void transformSharedToSync(SharedToSyncRequest sharedToSyncRequest) throws MetadataException;
	
	public GetBulkFileInfoResponse ambiguousSearch(GetSearchResultRequest getSearchResultRequest) throws MetadataException;

	public void cancelProcess(CancelProcessRequest cancelProcessRequest) throws MetadataException;
	
	public void setItemStyle(SetItemStyleRequest setItemStyleRequest) throws MetadataException;
	
	public void setSubItemsStyle(SetSubItemsStyleRequest setSubItemsStyleRequest) throws MetadataException;
}
