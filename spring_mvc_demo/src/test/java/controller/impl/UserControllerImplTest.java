package controller.impl;

import static org.junit.Assert.assertEquals;
import model.request.GetUserRequest;
import model.request.RestCommonRequest;
import model.request.RestUserRequest;
import model.request.UpdateSettingsRequest;
import model.response.GetUserResponse;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import service.UserService;
import constants.HttpStatus;
import exception.MetadataException;

public class UserControllerImplTest {
	@Mock
	private UserService userService;
	
	@InjectMocks
	private UserControllerImpl impl;
	
	@Before
	public void initMocks(){
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void getUserTest(){
		GetUserRequest request = Mockito.mock(GetUserRequest.class);
		
		GetUserResponse getUserResponse = Mockito.mock(GetUserResponse.class);
		Mockito.when(userService.getUser(request)).thenReturn(getUserResponse);
		GetUserResponse response = impl.getUser(request);
		assertEquals(response, response);
	}
	
	@Test
	public void restUserTest(){
		RestUserRequest request = Mockito.mock(RestUserRequest.class);
		
		Mockito.when(userService.reset(request)).thenReturn(true);
		impl.reset(request);
	}
	
	
	@Test
	public void restCommonBAD_REQUESTTest(){
		HttpStatus status = HttpStatus.OK;
		RestCommonRequest request = Mockito.mock(RestCommonRequest.class);
		try{
			impl.resetCommon(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void restCommonTest(){
		RestCommonRequest request = Mockito.mock(RestCommonRequest.class);
		
		Mockito.when(request.getPath()).thenReturn("roger");
		
		Mockito.when(userService.resetCommon(request)).thenReturn(true);
		impl.resetCommon(request);
	}
	
	@Test
	public void updateSettingsTest(){
		UpdateSettingsRequest request = Mockito.mock(UpdateSettingsRequest.class);
		impl.updateSettings(request);
	}
}
