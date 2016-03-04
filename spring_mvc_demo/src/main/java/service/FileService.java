package service;

import java.util.List;

import model.BaseMetadata;
import model.DefaultBackupSetting;
import model.ExistFile;
import model.Metadata;
import model.PathRequestBase;
import model.RequestBase;
import model.TextViewSetting;
import model.request.AddExtensionBackupRequest;
import model.request.BulkActionRequest;
import model.request.BulkActionRequestItem;
import model.request.BulkCreateFileRequest;
import model.request.CopyFileRequest;
import model.request.DeleteExtensionBackupRequest;
import model.request.EncryptRequest;
import model.request.ExistFileRequest;
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
import model.response.GetExtensionBackupResponse;
import model.response.GetFileProcessingStatusResponse;
import model.response.GetReaderFileMetadataResponse;
import model.response.NoteResponse;
import exception.MetadataException;

public interface FileService {

	public FileResponse getFile(GetFileRequest getFileRequest) throws MetadataException;

	public FileResponse createFile(UpdateFileRequest updateFileRequest) throws MetadataException;

	public FileResponse updateFile(UpdateFileRequest updateFileRequest) throws MetadataException;

	public List<BulkCreateFileResponse> bulkCreateFile(BulkCreateFileRequest bulkCreateFileRequestList) throws MetadataException;

	public FileResponse deleteFile(PathRequestBase deleteFileRequest) throws MetadataException;
	
	public void deleteFile(BaseMetadata baseMetadata);

	public FileResponse renameFile(RenameFileRequest rnameFileRequest) throws MetadataException;

	public FileResponse moveFile(MoveFileRequest moveFileRequest) throws MetadataException;

	public FileResponse copyFile(CopyFileRequest copyFileRequest) throws MetadataException;

	public void updateBackupCount(UpdateBackupCountRequest updateBackupCountRequest) throws MetadataException;

	public BackupCountResponse getBackupCount(PathRequestBase getBackupCountRequest) throws MetadataException;

	public Boolean updateNote(UpdateNoteRequest updateNoteRequest) throws MetadataException;

	public NoteResponse getNote(PathRequestBase getNoteRequest) throws MetadataException;

	public List<FileResponse> getBackupInfo(PathRequestBase getBackupInfoRequest) throws MetadataException;

	public GetReaderFileMetadataResponse getReaderFileMetadata(PathRequestBase getReaderFileMetadataRequest) throws MetadataException;

	public Boolean updateReaderFileMetadata(UpdateReaderFileMetadataRequest updateReaderFileMetadataRequest) throws MetadataException;

	public Boolean updateFileProcessingStatus(UpdateFileProcessingStatusRequest updateFileProcessingStatusRequest) throws MetadataException;

	public GetFileProcessingStatusResponse getFileProcessingStatus(GetFileProcessingStatusRequest getFileProcessingStatusRequest) throws MetadataException;
	
	public void encryptFile(EncryptRequest encryptRequest) throws MetadataException;
	
	public void afterEncrypt(UpdateFileRequest updateFileRequest) throws MetadataException;
	
	public void decryptFile(EncryptRequest decryptRequest) throws MetadataException;
	
	public void afterDecrypt(UpdateFileRequest updateFileRequest) throws MetadataException;
	
	public void encryptFileOld(EncryptRequest encryptRequest) throws MetadataException;
	
	public void decryptFileOld(EncryptRequest decryptRequest) throws MetadataException;
	
	public List<ExistFile> existFile(ExistFileRequest existFileRequest) throws MetadataException;
	
	public void updateImage(UpdateImageRequest updateImageRequest) throws MetadataException;

	public Metadata updateTargetFileBySourceFile(BaseMetadata sourceFile, Metadata targetFile, String userId, String targetOwnerId, String sharedRootId, String syncRootId, boolean fromBackup);

	public Metadata createTargetFileBySourceFile(BaseMetadata sourceFile, String targetParentId, String newName, String userId, String targetOwnerId, String sharedRootId, String syncRootId, boolean fromBackup);
	
	public GetBulkFileInfoResponse getBulkInfoByBlockId(GetBulkFileInfoRequest getBulkFileInfoRequest) throws MetadataException;
	
	public void deleteSingleFile(Metadata metadata);
	
	public Metadata copySingleFile(BaseMetadata sourceFile, Metadata targetFile, Metadata targetFolder, String userId, String targetOwnerId, boolean isOverride, boolean fromBackup);
	
	public void bulkUpdateLinkFile(BulkActionRequest<BulkActionRequestItem> bulkActionRequest) throws MetadataException;
	
	public TextViewSetting getTextViewSetting(GetTextViewSettingRequest getTextViewSettingRequest) throws MetadataException;
	
	public void updateTextViewSetting(UpdateTextViewSettingRequest updateTextViewSettingRequest) throws MetadataException;

	public void updateAlreadyRead(PathRequestBase pathRequestBase);

	public AlreadyReadResponse getAlreadyRead(PathRequestBase pathRequestBase);

	public List<FileResponse> updateAlreadyReadAndCount(String userId,BaseMetadata folder,List<FileResponse> fileResponses, boolean isFolder);
	
	public List<GetExtensionBackupResponse> getExtensionBackup(RequestBase getExtensionBackupRequest) throws MetadataException;

	public void addExtensionBackup(AddExtensionBackupRequest addExtensionBackupRequest);

	public void deleteExtensionBackup(DeleteExtensionBackupRequest deleteExtensionBackupRequest);

	public void setExtensionBackup(RequestBase requestBase,List<DefaultBackupSetting> updateExtensionBackupRequest);

	public void restoreBackup(RestoreBackupRequest restoreBackupRequest);

}
