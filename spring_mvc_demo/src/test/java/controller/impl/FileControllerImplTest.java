package controller.impl;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import model.PathRequestBase;
import model.request.BulkCreateFileRequest;
import model.request.CopyFileRequest;
import model.request.GetFileProcessingStatusRequest;
import model.request.GetFileRequest;
import model.request.MoveFileRequest;
import model.request.RenameFileRequest;
import model.request.UpdateBackupCountRequest;
import model.request.UpdateFileProcessingStatusRequest;
import model.request.UpdateFileRequest;
import model.request.UpdateNoteRequest;
import model.request.UpdateReaderFileMetadataRequest;
import model.response.BackupCountResponse;
import model.response.BackupResponse;
import model.response.BulkCreateFileResponse;
import model.response.FileResponse;
import model.response.GetFileProcessingStatusResponse;
import model.response.GetReaderFileMetadataResponse;
import model.response.NoteResponse;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import service.FileService;
import constants.HttpStatus;
import exception.MetadataException;

public class FileControllerImplTest {
	
	@Mock
	private FileService fileService;
	
	@InjectMocks
	private FileControllerImpl impl;
	
	@Before
	public void initMocks(){
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void getFileBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		GetFileRequest fileRequest = Mockito.mock(GetFileRequest.class);
		try{
			impl.getFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		try{
			impl.getFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getFileTest() throws MetadataException{
		GetFileRequest fileRequest = Mockito.mock(GetFileRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		FileResponse fileResponse = Mockito.mock(FileResponse.class);
		
		Mockito.when(fileService.getFile(fileRequest)).thenReturn(fileResponse);
		FileResponse response = impl.getFile(fileRequest);
		assertEquals(fileResponse, response);
	}
	
	@Test
	public void createFileBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateFileRequest fileRequest = null;
		try{
			fileRequest = Mockito.mock(UpdateFileRequest.class);
			impl.createFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("roger");
			impl.createFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void createFileTest() throws MetadataException{
		UpdateFileRequest fileRequest = Mockito.mock(UpdateFileRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getBlockId()).thenReturn("block_id");
		
		FileResponse fileResponse = Mockito.mock(FileResponse.class);
		
		Mockito.when(fileService.createFile(fileRequest)).thenReturn(fileResponse);
		FileResponse response = impl.createFile(fileRequest);
		assertEquals(fileResponse, response);
	}
	
	public void updateFileBlockBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateFileRequest updatfileRequest = null;
		try{
			updatfileRequest = Mockito.mock(UpdateFileRequest.class);
			impl.updateFile(updatfileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(updatfileRequest.getPath()).thenReturn("roger");
			impl.updateFile(updatfileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void updateFileBlockTest() throws MetadataException{
		UpdateFileRequest fileRequest = Mockito.mock(UpdateFileRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getBlockId()).thenReturn("block_id");
		Mockito.when(fileRequest.getSize()).thenReturn(10000L);
		
		FileResponse fileResponse = Mockito.mock(FileResponse.class);
		
		Mockito.when(fileService.updateFile(fileRequest)).thenReturn(fileResponse);
		FileResponse response = impl.updateFile(fileRequest);
		assertEquals(fileResponse, response);
	}
	
	public void bulkCreateFileBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		BulkCreateFileRequest fileRequestList = new BulkCreateFileRequest();
		UpdateFileRequest fileRequest = Mockito.mock(UpdateFileRequest.class);
		fileRequestList.add(fileRequest);
		try{	
			impl.bulkCreateFile(fileRequestList);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("roger");
			impl.bulkCreateFile(fileRequestList);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void bulkCreateTest() throws MetadataException{
		BulkCreateFileRequest fileRequestList = new BulkCreateFileRequest();
		UpdateFileRequest fileRequest = Mockito.mock(UpdateFileRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getBlockId()).thenReturn("block_id");
		
		List<BulkCreateFileResponse> fileResponse = new ArrayList<BulkCreateFileResponse>();
		
		Mockito.when(fileService.bulkCreateFile(fileRequestList)).thenReturn(fileResponse);
		List<BulkCreateFileResponse> response = impl.bulkCreateFile(fileRequestList);
		assertEquals(fileResponse, response);
	}
	
	@Test
	public void deleteFileBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase fileRequest = Mockito.mock(PathRequestBase.class);
		try{
			impl.deleteFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		try{
			impl.deleteFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void deleteFileTest() throws MetadataException{
		PathRequestBase fileRequest = Mockito.mock(PathRequestBase.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		FileResponse fileResponse = Mockito.mock(FileResponse.class);
		
		Mockito.when(fileService.deleteFile(fileRequest)).thenReturn(fileResponse);
		FileResponse response = impl.deleteFile(fileRequest);
		assertEquals(fileResponse, response);
	}
	
	@Test
	public void renameFileBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		RenameFileRequest fileRequest = Mockito.mock(RenameFileRequest.class);
		try{
			impl.renameFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		Mockito.when(fileRequest.getPath()).thenReturn("test");
		status = HttpStatus.OK;
		try{
			
			impl.renameFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		Mockito.when(fileRequest.getNewName()).thenReturn("test");
		status = HttpStatus.OK;
		try{
		
			impl.renameFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void renameFileTest() throws MetadataException{
		RenameFileRequest fileRequest = Mockito.mock(RenameFileRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getNewName()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		FileResponse fileResponse = Mockito.mock(FileResponse.class);
		
		Mockito.when(fileService.renameFile(fileRequest)).thenReturn(fileResponse);
		FileResponse response = impl.renameFile(fileRequest);
		assertEquals(fileResponse, response);
	}
	
	@Test
	public void moveFileBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		MoveFileRequest fileRequest = Mockito.mock(MoveFileRequest.class);
		try{
			impl.moveFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.moveFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void moveFileTest() throws MetadataException{
		MoveFileRequest fileRequest = Mockito.mock(MoveFileRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getDestination()).thenReturn("roger");
		
		FileResponse fileResponse = Mockito.mock(FileResponse.class);
		
		Mockito.when(fileService.moveFile(fileRequest)).thenReturn(fileResponse);
		FileResponse response = impl.moveFile(fileRequest);
		assertEquals(fileResponse, response);
	}
	
	@Test
	public void copyFileBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		CopyFileRequest fileRequest = Mockito.mock(CopyFileRequest.class);
		try{
			impl.copyFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.copyFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getDestination()).thenReturn("test");
			impl.copyFile(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void copyFileTest() throws MetadataException{
		CopyFileRequest fileRequest = Mockito.mock(CopyFileRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getDestination()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		FileResponse fileResponse = Mockito.mock(FileResponse.class);
		
		Mockito.when(fileService.copyFile(fileRequest)).thenReturn(fileResponse);
		FileResponse response = impl.copyFile(fileRequest);
		assertEquals(fileResponse, response);
	}
	
	@Test
	public void setBackupCountBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateBackupCountRequest fileRequest = Mockito.mock(UpdateBackupCountRequest.class);
		try{
			impl.updateBackupCount(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.updateBackupCount(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void setBackupCountTest() throws MetadataException{
		UpdateBackupCountRequest fileRequest = Mockito.mock(UpdateBackupCountRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		impl.updateBackupCount(fileRequest);
	}
	
	@Test
	public void getBackupCountBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase fileRequest = Mockito.mock(PathRequestBase.class);
		try{
			impl.getBackupCount(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.getBackupCount(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getBackupCountTest() throws MetadataException{
		PathRequestBase fileRequest = Mockito.mock(PathRequestBase.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		BackupCountResponse backupCountResponse = Mockito.mock(BackupCountResponse.class);
		Mockito.when(fileService.getBackupCount(fileRequest)).thenReturn(backupCountResponse);
		BackupCountResponse response = impl.getBackupCount(fileRequest);
		assertEquals(backupCountResponse, response);
	}
	
	@Test
	public void setNoteBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateNoteRequest fileRequest = Mockito.mock(UpdateNoteRequest.class);
		try{
			impl.updateNote(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.updateNote(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getSourcePath()).thenReturn("test");
			impl.updateNote(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void setNoteTest() throws MetadataException{
		UpdateNoteRequest fileRequest = Mockito.mock(UpdateNoteRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		Mockito.when(fileRequest.getNote()).thenReturn("");

		Mockito.when(fileService.updateNote(fileRequest)).thenReturn(true);
		impl.updateNote(fileRequest);
	}
	
	@Test
	public void getNoteBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase fileRequest = Mockito.mock(PathRequestBase.class);
		try{
			impl.getNote(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.getNote(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getNoteTest() throws MetadataException{
		PathRequestBase fileRequest = Mockito.mock(PathRequestBase.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		NoteResponse noteResponse = Mockito.mock(NoteResponse.class);
		Mockito.when(fileService.getNote(fileRequest)).thenReturn(noteResponse);
		NoteResponse response = impl.getNote(fileRequest);
		assertEquals(noteResponse, response);
	}
	
	@Test
	public void getBackupInfoBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase fileRequest = Mockito.mock(PathRequestBase.class);
		try{
			impl.getBackupInfo(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.getBackupInfo(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getBackupInfoTest() throws MetadataException{
		PathRequestBase fileRequest = Mockito.mock(PathRequestBase.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		List<FileResponse> backupResponse = new ArrayList<FileResponse>();
		Mockito.when(fileService.getBackupInfo(fileRequest)).thenReturn(backupResponse);
		List<FileResponse> response = impl.getBackupInfo(fileRequest);
		assertEquals(backupResponse, response);
	}
	
	@Test
	public void getReaderFileBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase readerFileRequest = Mockito.mock(PathRequestBase.class);
		try{
			impl.getReaderFileMetadata(readerFileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(readerFileRequest.getPath()).thenReturn("test");
			impl.getReaderFileMetadata(readerFileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getReaderFileTest() throws MetadataException{
		PathRequestBase readerFileRequest = Mockito.mock(PathRequestBase.class);
		Mockito.when(readerFileRequest.getPath()).thenReturn("roger");
		Mockito.when(readerFileRequest.getSourcePath()).thenReturn("roger");
		
		GetReaderFileMetadataResponse readerFileResponse = Mockito.mock(GetReaderFileMetadataResponse.class);
		Mockito.when(fileService.getReaderFileMetadata(readerFileRequest)).thenReturn(readerFileResponse);
		GetReaderFileMetadataResponse response = impl.getReaderFileMetadata(readerFileRequest);
		assertEquals(readerFileResponse, response);
	}
	
	@Test
	public void updateReaderFileBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateReaderFileMetadataRequest fileRequest = Mockito.mock(UpdateReaderFileMetadataRequest.class);
		try{
			impl.updateReaderFileMetadata(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.updateReaderFileMetadata(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void updateReaderFileTest() throws MetadataException{
		UpdateReaderFileMetadataRequest fileRequest = Mockito.mock(UpdateReaderFileMetadataRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		Mockito.when(fileService.updateReaderFileMetadata(fileRequest)).thenReturn(true);
		impl.updateReaderFileMetadata(fileRequest);
	}
	
	@Test
	public void setFileProcessingStatusBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateFileProcessingStatusRequest fileRequest = Mockito.mock(UpdateFileProcessingStatusRequest.class);
		try{
			impl.updateFileProcessingStatus(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.updateFileProcessingStatus(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void setFileProcessingStatusTest() throws MetadataException{
		UpdateFileProcessingStatusRequest fileRequest = Mockito.mock(UpdateFileProcessingStatusRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		Mockito.when(fileService.updateFileProcessingStatus(fileRequest)).thenReturn(true);
		impl.updateFileProcessingStatus(fileRequest);
	}
	
	@Test
	public void getFileProcessingStatusBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		GetFileProcessingStatusRequest fileRequest = Mockito.mock(GetFileProcessingStatusRequest.class);
		try{
			impl.getFileProcessingStatus(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		try{
			Mockito.when(fileRequest.getPath()).thenReturn("test");
			impl.getFileProcessingStatus(fileRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getFileProcessingStatusTest() throws MetadataException{
		GetFileProcessingStatusRequest fileRequest = Mockito.mock(GetFileProcessingStatusRequest.class);
		Mockito.when(fileRequest.getPath()).thenReturn("roger");
		Mockito.when(fileRequest.getSourcePath()).thenReturn("roger");
		
		GetFileProcessingStatusResponse responseEntity = new GetFileProcessingStatusResponse();
		Mockito.when(fileService.getFileProcessingStatus(fileRequest)).thenReturn(responseEntity);
		GetFileProcessingStatusResponse response = impl.getFileProcessingStatus(fileRequest);
		assertEquals(responseEntity, response);
	}
	
}
