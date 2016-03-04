package controller.impl;

import java.util.List;

import model.DefaultBackupSetting;
import model.ExistFile;
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

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import service.FileService;
import utils.StringUtils;
import annotation.AllowAdminAuthorization;
import annotation.Readonly;
import constants.HttpStatus;
import controller.FileContorller;
import exception.MetadataException;

@RestController
public class FileControllerImpl implements FileContorller {
	
	@Autowired
	private FileService fileService;

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/info", method = RequestMethod.POST)
	public FileResponse getFile(@RequestBody GetFileRequest getFileRequest)
			throws MetadataException { 
		if (StringUtils.isNullOrEmpty(getFileRequest.getPath())
				|| getFileRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.getFile(getFileRequest);
	}
	
	
	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/bulk_info_by_block_id", method = RequestMethod.POST)
	public GetBulkFileInfoResponse getBulkInfoByBlockId(@RequestBody GetBulkFileInfoRequest getBulkFileInfoRequest)
			throws MetadataException { 
		if (getBulkFileInfoRequest == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.getBulkInfoByBlockId(getBulkFileInfoRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/create", method = RequestMethod.POST)
	public FileResponse createFile(@RequestBody UpdateFileRequest updateFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFileRequest.getPath())
				|| StringUtils.isNullOrEmpty(updateFileRequest.getBlockId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.createFile(updateFileRequest);
	}
	
	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/update_file", method = RequestMethod.POST)
	public FileResponse updateFile(@RequestBody UpdateFileRequest updateFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFileRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.updateFile(updateFileRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/delete", method = RequestMethod.POST)
	public FileResponse deleteFile(@RequestBody PathRequestBase deleteFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(deleteFileRequest.getPath())
				|| deleteFileRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.deleteFile(deleteFileRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/file/rename", method = RequestMethod.POST)
	public FileResponse renameFile(@RequestBody RenameFileRequest renameFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(renameFileRequest.getPath())
				|| StringUtils.isNullOrEmpty(renameFileRequest.getNewName())
				|| renameFileRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.renameFile(renameFileRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/file/move", method = RequestMethod.POST)
	public FileResponse moveFile(@RequestBody MoveFileRequest moveFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(moveFileRequest.getPath())
				|| moveFileRequest.getDestination() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.moveFile(moveFileRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/file/copy", method = RequestMethod.POST)
	public FileResponse copyFile(@RequestBody CopyFileRequest copyFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(copyFileRequest.getPath())
				|| copyFileRequest.getDestination() == null
				|| copyFileRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.copyFile(copyFileRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/file/backupcount", method = RequestMethod.POST)
	public void updateBackupCount(@RequestBody UpdateBackupCountRequest updateBackupCountRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateBackupCountRequest.getPath())
				|| updateBackupCountRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.updateBackupCount(updateBackupCountRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/file/getbackupcount", method = RequestMethod.POST)
	public BackupCountResponse getBackupCount(@RequestBody PathRequestBase getBackupCountRequest)
			throws MetadataException {
		if (getBackupCountRequest.getPath() == null
			|| getBackupCountRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.getBackupCount(getBackupCountRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/file/note", method = RequestMethod.POST)
	public void updateNote(@RequestBody UpdateNoteRequest updateNoteRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateNoteRequest.getPath())
			|| updateNoteRequest.getSourcePath() == null
			|| updateNoteRequest.getNote() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.updateNote(updateNoteRequest);
	}

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/get_note", method = RequestMethod.POST)
	public NoteResponse getNote(@RequestBody PathRequestBase getNoteRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(getNoteRequest.getPath())
				|| getNoteRequest.getSourcePath() == null) {
				throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
			}
			return fileService.getNote(getNoteRequest);
	}

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/backup_info", method = RequestMethod.POST)
	public List<FileResponse> getBackupInfo(@RequestBody PathRequestBase getBackupInfoRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(getBackupInfoRequest.getPath())
				|| getBackupInfoRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.getBackupInfo(getBackupInfoRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/file/get_reader_file_metadata", method = RequestMethod.POST)
	public GetReaderFileMetadataResponse getReaderFileMetadata(@RequestBody PathRequestBase getReaderFileMetadataRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(getReaderFileMetadataRequest.getPath())
				|| getReaderFileMetadataRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.getReaderFileMetadata(getReaderFileMetadataRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/update_reader_file_metadata", method = RequestMethod.POST)
	public void updateReaderFileMetadata(@RequestBody UpdateReaderFileMetadataRequest updateReaderFileMetadataRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateReaderFileMetadataRequest.getPath())
				|| updateReaderFileMetadataRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.updateReaderFileMetadata(updateReaderFileMetadataRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/update_processing_status", method = RequestMethod.POST)
	public void updateFileProcessingStatus(
			@RequestBody UpdateFileProcessingStatusRequest updateFileProcessingStatusRequest) throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFileProcessingStatusRequest.getPath())
				|| updateFileProcessingStatusRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.updateFileProcessingStatus(updateFileProcessingStatusRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/bulk_create", method = RequestMethod.POST)
	public List<BulkCreateFileResponse> bulkCreateFile(
			@RequestBody  BulkCreateFileRequest bulkCreateFileRequestList)
			throws MetadataException {
		for(UpdateFileRequest fileRequest : bulkCreateFileRequestList){
			if (StringUtils.isNullOrEmpty(fileRequest.getPath())
					|| StringUtils.isNullOrEmpty(fileRequest.getBlockId())) {
				throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
			}
		}
		
		return fileService.bulkCreateFile(bulkCreateFileRequestList);
	}

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/get_processing_status", method = RequestMethod.POST)
	public GetFileProcessingStatusResponse getFileProcessingStatus(
			@RequestBody GetFileProcessingStatusRequest getFileProcessingStatusRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(getFileProcessingStatusRequest.getPath())
				|| getFileProcessingStatusRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.getFileProcessingStatus(getFileProcessingStatusRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/file/encrypt", method = RequestMethod.POST)
	public void encryptFile(
			@RequestBody EncryptRequest encryptRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(encryptRequest.getPath())
				|| encryptRequest.getSourcePath() == null
				|| StringUtils.isNullOrEmpty(encryptRequest.getPassword())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.encryptFile(encryptRequest);
	}

	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/after_encrypt", method = RequestMethod.POST)
	public void afterEncrypt(@RequestBody UpdateFileRequest updateFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFileRequest.getPath())
				|| StringUtils.isNullOrEmpty(updateFileRequest.getBlockId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.afterEncrypt(updateFileRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/file/decrypt", method = RequestMethod.POST)
	public void decryptFile(
			@RequestBody EncryptRequest decryptRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(decryptRequest.getPath())
				|| decryptRequest.getSourcePath() == null
				|| StringUtils.isNullOrEmpty(decryptRequest.getPassword())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.decryptFile(decryptRequest);
	}
	
	@Override
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/file/after_decrypt", method = RequestMethod.POST)
	public void afterDecrypt(@RequestBody UpdateFileRequest updateFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateFileRequest.getPath())
				|| StringUtils.isNullOrEmpty(updateFileRequest.getBlockId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.afterDecrypt(updateFileRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/file/encrypt_old", method = RequestMethod.POST)
	public void encryptFileOld(
			@RequestBody EncryptRequest encryptRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(encryptRequest.getPath())
				|| encryptRequest.getSourcePath() == null
				|| StringUtils.isNullOrEmpty(encryptRequest.getPassword())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.encryptFileOld(encryptRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/file/decrypt_old", method = RequestMethod.POST)
	public void decryptFileOld(
			@RequestBody EncryptRequest decryptRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(decryptRequest.getPath())
				|| decryptRequest.getSourcePath() == null
				|| StringUtils.isNullOrEmpty(decryptRequest.getPassword())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.decryptFileOld(decryptRequest);
	}
	
	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/file/exist", method = RequestMethod.POST)
	public List<ExistFile> existFile(
			@RequestBody ExistFileRequest existFileRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(existFileRequest.getItems())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.existFile(existFileRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/file/update_image", method = RequestMethod.POST)
	public void updateImage(@RequestBody UpdateImageRequest updateImageRequest)
			throws MetadataException {
		if(StringUtils.isNullOrEmpty(updateImageRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if(updateImageRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if((updateImageRequest.getWidth() == null && updateImageRequest.getHeight() != null)
				|| (updateImageRequest.getWidth() != null && updateImageRequest.getHeight() == null)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.updateImage(updateImageRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/file/bulk_update_linkfile", method = RequestMethod.POST)
	public void bulkUpdateLinkFile(@RequestBody BulkActionRequest<BulkActionRequestItem> bulkActionRequest)
			throws MetadataException {
		fileService.bulkUpdateLinkFile(bulkActionRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/file/text_view_setting", method = RequestMethod.POST)
	public TextViewSetting getTextViewSetting(@RequestBody GetTextViewSettingRequest getTextViewSettingRequest)
			throws MetadataException {
		if(StringUtils.isNullOrEmpty(getTextViewSettingRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if(getTextViewSettingRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.getTextViewSetting(getTextViewSettingRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/file/update_text_view_setting", method = RequestMethod.POST)
	public void updateTextViewSetting(@RequestBody UpdateTextViewSettingRequest updateTextViewSettingRequest)
			throws MetadataException {
		if(StringUtils.isNullOrEmpty(updateTextViewSettingRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if(updateTextViewSettingRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.updateTextViewSetting(updateTextViewSettingRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/file/already_read", method = RequestMethod.POST)
	public void updateAlreadyRead(@RequestBody PathRequestBase pathRequestBase)
			throws MetadataException {
		if(StringUtils.isNullOrEmpty(pathRequestBase.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.updateAlreadyRead(pathRequestBase);
	}
	
	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/file/get_already_read", method = RequestMethod.POST)
	public AlreadyReadResponse getAlreadyRead(@RequestBody PathRequestBase pathRequestBase)
			throws MetadataException {
		if(StringUtils.isNullOrEmpty(pathRequestBase.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if(StringUtils.isNullOrEmpty(pathRequestBase.getSharedRootId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return fileService.getAlreadyRead(pathRequestBase);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/file/get_extension_backup", method = RequestMethod.POST)
	public List<GetExtensionBackupResponse> getExtensionBackup(@RequestBody RequestBase getExtensionBackupRequest)
			throws MetadataException {
		return fileService.getExtensionBackup(getExtensionBackupRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/file/add_extension_backup", method = RequestMethod.POST)
	public void addExtensionBackup(@RequestBody AddExtensionBackupRequest addExtensionBackupRequest)
			throws MetadataException {
		if(StringUtils.isNullOrEmpty(addExtensionBackupRequest.getExtension())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		fileService.addExtensionBackup(addExtensionBackupRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/file/delete_extension_backup", method = RequestMethod.POST)
	public void deleteExtensionBackup(@RequestBody DeleteExtensionBackupRequest deleteExtensionBackupRequest)
			throws MetadataException {
		fileService.deleteExtensionBackup(deleteExtensionBackupRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/file/set_extension_backup", method = RequestMethod.POST)
	public void setExtensionBackup(RequestBase requestBase,@RequestBody List<DefaultBackupSetting> updateExtensionBackupRequest)
			throws MetadataException {
		fileService.setExtensionBackup(requestBase,updateExtensionBackupRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/file/restore_backup", method = RequestMethod.POST)
	public void restoreBackup(@RequestBody RestoreBackupRequest restoreBackupRequest)
			throws MetadataException {
		fileService.restoreBackup(restoreBackupRequest);
	}
	
}
