package controller.impl;

import static org.junit.Assert.assertEquals;
import model.request.GetDeviceRequest;
import model.request.UpdateDeviceRequest;
import model.response.GetAllDeviceResponse;
import model.response.GetDeviceResponse;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import service.DeviceService;
import constants.HttpStatus;
import exception.MetadataException;

public class DeviceControllerImplTest {
	@Mock
	private DeviceService service;
	
	@InjectMocks
	private DeviceControllerImpl impl;
	
	@Before
	public void initMocks(){
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void getDeviceTest(){
		GetDeviceRequest request = new GetDeviceRequest();
		request.setDeviceUid("testid");
		GetDeviceResponse getDeviceResponse = Mockito.mock(GetDeviceResponse.class);
		Mockito.when(service.getDevice(request)).thenReturn(getDeviceResponse);
		GetDeviceResponse response = (GetDeviceResponse)impl.getDevice(request);
		assertEquals(getDeviceResponse,response);
		assertEquals(request.getDeviceUid(),"testid");
	}
	
	@Test
	public void udpateDeviceBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		UpdateDeviceRequest request = Mockito.mock(UpdateDeviceRequest.class);
		
		try{
			impl.updateDevice(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void udpateDeviceTest(){
		UpdateDeviceRequest request = Mockito.mock(UpdateDeviceRequest.class);
		Mockito.when(request.getDeviceUid()).thenReturn("roger");
		
		Mockito.when(service.updateDevice(request)).thenReturn(true);
		impl.updateDevice(request);
	}
	
	@Test
	public void getAllDeviceTest(){
		GetDeviceRequest request = Mockito.mock(GetDeviceRequest.class);
		
		GetAllDeviceResponse getAllDeviceResponse = new GetAllDeviceResponse();
		
		Mockito.when(service.getAllDevice(request)).thenReturn(getAllDeviceResponse);
		GetAllDeviceResponse response = (GetAllDeviceResponse) impl.getDevice(request);
		assertEquals(response, getAllDeviceResponse);
	}
	
	
}
