package controller.impl;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import model.PathRequestBase;
import model.PriorityInfo;
import model.request.CopyFolderRequest;
import model.request.DeleteFolderBackupRequest;
import model.request.DeleteFolderRequest;
import model.request.GetFolderRequest;
import model.request.GetGlobalIconRequest;
import model.request.LinkFoldersRequest;
import model.request.MoveFolderRequest;
import model.request.RenameFolderRequest;
import model.request.UnzipRequest;
import model.request.UpdateFolderBackupCountRequest;
import model.request.UpdateFolderNoteRequest;
import model.request.UpdateFolderRequest;
import model.request.UpdateIconRequest;
import model.request.UpdatePriorityRequest;
import model.request.ZipRequest;
import model.request.ZipSource;
import model.response.GetFolderBackupCountResponse;
import model.response.GetFolderNoteResponse;
import model.response.GetFolderResponse;
import model.response.GetGlobalIconResponse;
import model.response.GetSubBackupCountResponse;
import model.response.LinkFoldersResponse;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import service.FolderService;
import constants.HttpStatus;
import exception.MetadataException;
import factory.PathFactory;

public class FolderControllerImplTest {
	@Mock
	private FolderService folderService;
	
	@Mock
	private PathFactory pathFactory;
	
	@InjectMocks
	private FolderControllerImpl impl;
	
	@Before
	public void initMocks(){
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void getFolderBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		GetFolderRequest request = Mockito.mock(GetFolderRequest.class);
		try{
			impl.getFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getFolderTest(){
		GetFolderRequest request = Mockito.mock(GetFolderRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		GetFolderResponse response = Mockito.mock(GetFolderResponse.class);
		Mockito.when(folderService.getFolder(request)).thenReturn(response);
		GetFolderResponse getFolderResposne = impl.getFolder(request);
		assertEquals(response,getFolderResposne);
	}
	
	@Test
	public void createFolderBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateFolderRequest request = Mockito.mock(UpdateFolderRequest.class);
		try{
			impl.createFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void createFolderTest(){
		UpdateFolderRequest request = Mockito.mock(UpdateFolderRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		Mockito.when(folderService.createFolder(request)).thenReturn(true);
		impl.createFolder(request);
	}
	
	@Test
	public void updateFolderBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		try{
			impl.updateFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void updateFolderTest(){
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		Mockito.when(folderService.updateFolder(request)).thenReturn(true);
		impl.updateFolder(request);
	}
	
	@Test
	public void deleteFolderBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		DeleteFolderRequest request = Mockito.mock(DeleteFolderRequest.class);
		try{
			impl.deleteFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void deleteFolderTest(){
		DeleteFolderRequest request = Mockito.mock(DeleteFolderRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
//		Mockito.when(folderService.deleteFolder(request)).thenReturn(true);
		impl.deleteFolder(request);
	}
	
	@Test
	public void renameFolderBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		RenameFolderRequest request = Mockito.mock(RenameFolderRequest.class);
		try{
			impl.renameFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		Mockito.when(request.getPath()).thenReturn("roger");
		try{
			impl.renameFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void renameFolderTest(){
		RenameFolderRequest request = Mockito.mock(RenameFolderRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		Mockito.when(request.getNewName()).thenReturn("roger");
		
		impl.renameFolder(request);
	}
	
	@Test
	public void updateBackupCountBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateFolderBackupCountRequest request = Mockito.mock(UpdateFolderBackupCountRequest.class);
		try{
			impl.updateBackupCount(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void updateBackupCountTest(){
		UpdateFolderBackupCountRequest request = Mockito.mock(UpdateFolderBackupCountRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		impl.updateBackupCount(request);
	}
	
	@Test
	public void getBackupCountBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		try{
			impl.getBackupCount(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getBackupCountTest(){
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		GetFolderBackupCountResponse getFolderBackupCountResponse = Mockito.mock(GetFolderBackupCountResponse.class);
		Mockito.when(folderService.getBackupCount(request)).thenReturn(getFolderBackupCountResponse);
		GetFolderBackupCountResponse response = impl.getBackupCount(request);
		assertEquals(getFolderBackupCountResponse,response);
	}
	
	@Test
	public void updatePriorityBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdatePriorityRequest request = new UpdatePriorityRequest();
		List<PriorityInfo> priorities = new ArrayList<PriorityInfo>();
		PriorityInfo priorityInfo = new PriorityInfo();
		priorities.add(priorityInfo);
		request.setPriority(priorities);
		
		try{
			impl.updatePriority(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		
		request.setPath("roger");
		
		try{
			impl.updatePriority(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		
		priorityInfo.setName("roger");
		
		try{
			impl.updatePriority(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;	
	}
	
	@Test
	public void updatePriorityTest(){
		UpdatePriorityRequest request = new UpdatePriorityRequest();
		List<PriorityInfo> priorities = new ArrayList<PriorityInfo>();
		PriorityInfo priorityInfo = new PriorityInfo();
		priorities.add(priorityInfo);
		request.setPriority(priorities);
		
		request.setPath("roger");
		priorityInfo.setName("roger");
		priorityInfo.setSourcePath("roger");
		
		Mockito.when(folderService.updatePriority(request)).thenReturn(true);
		impl.updatePriority(request);
	}

	@Test
	public void unlikFolderBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		try{
			impl.unlinkFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void unlinkFolderTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		try{
			impl.unlinkFolder(request);
		}
		catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.OK);
	}
	
	@Test
	public void linkFolderBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		try{
			impl.linkFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void linkFolderTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		try{
			impl.linkFolder(request);
		}
		catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.OK);
	}
	
	@Test
	public void linkFoldersBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		LinkFoldersRequest request = Mockito.mock(LinkFoldersRequest.class);
		try{
			impl.linkFolders(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void linkFoldersTest(){
		LinkFoldersRequest request = Mockito.mock(LinkFoldersRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		LinkFoldersResponse response = Mockito.mock(LinkFoldersResponse.class);
		
		Mockito.when(folderService.linkFolders(request)).thenReturn(response);
		LinkFoldersResponse linkFoldersResponse = impl.linkFolders(request);
		assertEquals(response,linkFoldersResponse);
	}
	
	@Test
	public void updateFolderNoteBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateFolderNoteRequest request = Mockito.mock(UpdateFolderNoteRequest.class);
		try{
			impl.updateFolderNote(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void updateFolderNoteTest(){
		UpdateFolderNoteRequest request = Mockito.mock(UpdateFolderNoteRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		Mockito.when(folderService.updateFolderNote(request)).thenReturn(true);
		impl.updateFolderNote(request);
	}
	

	@Test
	public void getFolderNoteBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		try{
			impl.getFolderNote(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getFolderNoteTest(){
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		GetFolderNoteResponse getFolderNoteResponse = Mockito.mock(GetFolderNoteResponse.class);
		Mockito.when(folderService.getFolderNote(request)).thenReturn(getFolderNoteResponse);
		GetFolderNoteResponse response = impl.getFolderNote(request);
		assertEquals(getFolderNoteResponse,response);
	}
	
	@Test
	public void deleteFolderBackupBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		DeleteFolderBackupRequest request = Mockito.mock(DeleteFolderBackupRequest.class);
		try{
			impl.deleteFolderBackup(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void deleteFolderBackupTest(){
		DeleteFolderBackupRequest request = Mockito.mock(DeleteFolderBackupRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		//Mockito.when(folderService.deleteFolderBackup(request)).thenReturn(true);
		impl.deleteFolderBackup(request);
	}
	
	@Test
	public void getSubBackupCountBackupBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		try{
			impl.getSubBackupCount(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getSubBackupCountTest(){
		PathRequestBase request = Mockito.mock(PathRequestBase.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		List<GetSubBackupCountResponse> getSubBackupCountRequest = new ArrayList<GetSubBackupCountResponse>();
		Mockito.when(folderService.getSubBackupCount(request)).thenReturn(getSubBackupCountRequest);
		List<GetSubBackupCountResponse> response = impl.getSubBackupCount(request);
		assertEquals(getSubBackupCountRequest,response);
	}
	
	@Test
	public void zipBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		
		ZipRequest request = new ZipRequest();
		List<ZipSource> zipSources = new ArrayList<ZipSource>();
		ZipSource zipSource = new ZipSource();
		zipSources.add(zipSource);
		request.setZipSource(zipSources);
		
		request.setTargetPath("roger");
		
		try{
			impl.zip(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		zipSource.setPath("roger");
		
		try{
			impl.zip(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		zipSource.setPath("roger");
		
		try{
			impl.zip(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
	}
	
	@Test
	public void zipTest(){
	
	}
	
	@Test
	public void unzipBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UnzipRequest request = Mockito.mock(UnzipRequest.class);
		try{
			impl.unzip(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;

		Mockito.when(request.getTargetPath()).thenReturn("roger");
		try{
			impl.unzip(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void unzipTest(){

	}
	
	@Test
	public void MoveFolderBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		MoveFolderRequest request = Mockito.mock(MoveFolderRequest.class);
		try{
			impl.moveFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void moveFolderTest(){
		MoveFolderRequest request = Mockito.mock(MoveFolderRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		Mockito.when(request.getDestination()).thenReturn("roger");
		impl.moveFolder(request);
	}
	
	@Test
	public void CopyFolderBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		CopyFolderRequest request = Mockito.mock(CopyFolderRequest.class);
		try{
			impl.copyFolder(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void copyFolderTest(){
		CopyFolderRequest request = Mockito.mock(CopyFolderRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		Mockito.when(request.getDestination()).thenReturn("roger");
		impl.copyFolder(request);
	}
	
	@Test
	public void getGlobalIconTest(){
		GetGlobalIconRequest request = Mockito.mock(GetGlobalIconRequest.class);
		
		Mockito.when(request.isIncludeUserDefinedIcon()).thenReturn(true);
		GetGlobalIconResponse getGlobalIconResponse = new GetGlobalIconResponse();

		Mockito.when(folderService.getGlobalIcon(request)).thenReturn(getGlobalIconResponse);
		GetGlobalIconResponse response = impl.getGlobalIcon(request);
		assertEquals(getGlobalIconResponse,response);
	}
	
	@Test
	public void updateIconTest(){
		
		UpdateIconRequest request = Mockito.mock(UpdateIconRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		Mockito.when(request.getBlockId()).thenReturn("roger");
		impl.updateIcon(request);
	}

}
