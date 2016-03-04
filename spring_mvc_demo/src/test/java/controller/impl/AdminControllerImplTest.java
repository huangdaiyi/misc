package controller.impl;

import static org.junit.Assert.assertEquals;
import model.request.AddShareLinkRequest;
import model.request.ConfirmShareLinkRequest;
import model.request.DeleteShareLinkRequest;
import model.request.ExistsShareLinkRequest;
import model.request.GetShareLinkRequest;
import model.request.SendAllMetadataToMQRequest;
import model.request.UpdateShareLinkRequest;
import model.response.AddShareLinkResponse;
import model.response.ConfirmShareLinkResponse;
import model.response.ExistsShareLinkResponse;
import model.response.GetShareLinkResponse;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import service.AdminService;
import service.ShareLinkService;
import constants.HttpStatus;
import exception.MetadataException;

public class AdminControllerImplTest {
	@Mock
	private AdminService adminService;
	@Mock
	private ShareLinkService shareLinkService;
	
	@InjectMocks
	private AdminControllerImpl impl;
	
	@Before
	public void initMocks(){
		MockitoAnnotations.initMocks(this);
	}
	

	@Test
	public void addShareLinkBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		AddShareLinkRequest request = Mockito.mock(AddShareLinkRequest.class);
		try{
			impl.addShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
		Mockito.when(request.getPath()).thenReturn("roger");
		try{
			impl.addShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void addShareLinkTest(){
		AddShareLinkRequest request = Mockito.mock(AddShareLinkRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		Mockito.when(request.getSourcePath()).thenReturn("roger");
		
		AddShareLinkResponse addShareLinkResponse = Mockito.mock(AddShareLinkResponse.class);

		Mockito.when(shareLinkService.addShareLink(request)).thenReturn(addShareLinkResponse);
		AddShareLinkResponse response = impl.addShareLink(request);
		assertEquals(addShareLinkResponse,response);
	}
	
	
	@Test
	public void getShareLinkBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		GetShareLinkRequest request = Mockito.mock(GetShareLinkRequest.class);
		
		try{
			impl.getShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void getShareLinkTest(){
		GetShareLinkRequest request = Mockito.mock(GetShareLinkRequest.class);
		Mockito.when(request.getLink()).thenReturn("roger");
		
		GetShareLinkResponse getShareLinkResponse = Mockito.mock(GetShareLinkResponse.class);

		Mockito.when(shareLinkService.getShareLink(request)).thenReturn(getShareLinkResponse);
		GetShareLinkResponse response = impl.getShareLink(request);
		assertEquals(getShareLinkResponse,response);
	}
	
	@Test
	public void existsShareLinkBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		ExistsShareLinkRequest request = Mockito.mock(ExistsShareLinkRequest.class);
		
		try{
			impl.existsShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
			
		status = HttpStatus.OK;
		Mockito.when(request.getPath()).thenReturn("roger");
		try{
			impl.existsShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void existsShareLinkTest(){
		ExistsShareLinkRequest request = Mockito.mock(ExistsShareLinkRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		Mockito.when(request.getSourcePath()).thenReturn("roger");
		
		ExistsShareLinkResponse existsShareLinkResponse = Mockito.mock(ExistsShareLinkResponse.class);

		Mockito.when(shareLinkService.existsShareLink(request)).thenReturn(existsShareLinkResponse);
		ExistsShareLinkResponse response = impl.existsShareLink(request);
		assertEquals(existsShareLinkResponse,response);
	}
	
	@Test
	public void confirmShareLinkBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		ConfirmShareLinkRequest request = Mockito.mock(ConfirmShareLinkRequest.class);
		
		try{
			impl.confirmShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
			
		status = HttpStatus.OK;
		Mockito.when(request.getLink()).thenReturn("roger");
		try{
			impl.confirmShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void confirmShareLinkTest(){
		ConfirmShareLinkRequest request = Mockito.mock(ConfirmShareLinkRequest.class);
		Mockito.when(request.getLink()).thenReturn("roger");
		Mockito.when(request.getPassword()).thenReturn("roger");
		
		ConfirmShareLinkResponse confirmShareLinkResponse = Mockito.mock(ConfirmShareLinkResponse.class);

		Mockito.when(shareLinkService.confirmShareLink(request)).thenReturn(confirmShareLinkResponse);
		ConfirmShareLinkResponse response = impl.confirmShareLink(request);
		assertEquals(confirmShareLinkResponse,response);
	}
	
	@Test
	public void deleteShareLinkBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		DeleteShareLinkRequest request = Mockito.mock(DeleteShareLinkRequest.class);
		
		try{
			impl.deleteShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
			
		status = HttpStatus.OK;
		Mockito.when(request.getPath()).thenReturn("roger");
		try{
			impl.deleteShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void deleteShareLinkTest(){
		DeleteShareLinkRequest request = Mockito.mock(DeleteShareLinkRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		Mockito.when(request.getSourcePath()).thenReturn("roger");
		
		//Mockito.when(shareLinkService.deleteShareLink(request)).thenReturn(true);
		impl.deleteShareLink(request);
	}
	
	@Test
	public void updateShareLinkBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateShareLinkRequest request = Mockito.mock(UpdateShareLinkRequest.class);
		
		try{
			impl.updateShareLink(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void updateShareLinkTest(){
		UpdateShareLinkRequest request = Mockito.mock(UpdateShareLinkRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		Mockito.when(shareLinkService.updateShareLink(request)).thenReturn(true);
		impl.updateShareLink(request);
	}
	
	@Test
	public void sendAllMetadataToMQTest(){
		SendAllMetadataToMQRequest request = Mockito.mock(SendAllMetadataToMQRequest.class);
		
		Mockito.when(adminService.sendAllMetadataToMQ(request)).thenReturn(true);
		impl.sendAllMetadataToMQ(request);
	}
	
}
