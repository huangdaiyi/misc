package service.impl;

import static org.junit.Assert.assertEquals;

import java.util.List;

import model.CellPhone;
import model.Device;
import model.RequestBase;
import model.request.SSOUserInfoRequest;
import model.response.SSODevicesInfo;
import model.response.SSOUserInfoResponse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;

import service.RestService;
import service.SSOService;
import service.UserAuthorizationService;
import base.BaseTestCase;
import constants.HttpStatus;
import exception.MetadataException;

public class SSOServiceImplTest extends BaseTestCase {

	@Autowired
	@InjectMocks
	private SSOService ssoService;

	@Value("${sso.host}")
	private String host;
	@Value("${sso.user_info}")
	private String userInfo;
	@Value("${sso.servcieName}")
	private String servcieName;

	private String token = "d04f88283256697b79fa19003dd12be5a2e46bbe";

	private String response = "{\"status\":\"success123\",\"id\":\"fc1fd9e71de4f8c53bb7d2f1182772d3\",\"display_name\":\"mervin2\","
			+ "\"name\":\"mervin2\",\"cellphones\":[{\"cellphone\":\"0900002230\",\"country_code\":\"886\"},{\"cellphone\":\"0900002231\","
			+ "\"country_code\":\"886\"}],\"device_id\":null,\"expired\":\"\",\"inactive_cellphone\":[],\"profile_image\":null}";

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testCheckToken() {
		RequestBase requestBase = ((UserAuthorizationService) ssoService).checkToken("f2e5ab424ed392551533e5444251bf4028de457e");
		Assert.assertEquals("73450575e5c049baf03e53d3540edd64", requestBase.getUserId());
		Assert.assertEquals("mydevice", requestBase.getDeviceId());
	}

	@Test
	public void getSSOUserInfoTestMock() {
		RestService restService = Mockito
				.mock(RestService.class);

		SSOUserInfoRequest ssoUserInfo = new SSOUserInfoRequest();
		ssoUserInfo.setToken(token);
		Mockito.when(
				restService.sendPostRequestForObj(host + userInfo,
						addHttpHeaders(), ssoUserInfo, String.class))
				.thenReturn(response);
		SSOUserInfoResponse ssoUserInfoResponse = ssoService
				.getUserInfoByToken(ssoUserInfo);
		Assert.assertNotNull(ssoUserInfoResponse);

	}

	@Test
	public void getSSOUserInfoBad_RequestTest() {
		RestService restService = Mockito
				.mock(RestService.class);
		HttpStatus errorCode = null;
		SSOUserInfoRequest ssoUserInfo = new SSOUserInfoRequest();
		Mockito.when(
				restService.sendPostRequestForObj("", addHttpHeaders(),
						ssoUserInfo, String.class)).thenReturn(null);
		try {
			ssoService.getUserInfoByToken(ssoUserInfo);
		} catch (MetadataException ex) {
			errorCode = ex.getHttpStatus();
		}
		assertEquals(HttpStatus.ERROR_IN_PARAMETERS, errorCode);

	}

	@Test
	public void getSSOUserInfoTest() {
		RequestBase ssoUserInfo = new RequestBase();
		ssoUserInfo.setToken(token);
		SSOUserInfoResponse ssoUserInfoResponse = ssoService
				.getUserInfoByToken(ssoUserInfo);
		Assert.assertNotNull(ssoUserInfoResponse);

	}
	@Test
	public void getSSODevicesInfoTest(){
		RequestBase request = new RequestBase();
		request.setToken(token);
		SSODevicesInfo response  = ssoService.getDevicesByToken(request);
		Assert.assertNotNull(response);
	}

	private HttpHeaders addHttpHeaders() {
		HttpHeaders headers = new HttpHeaders();
		headers.add("ServiceName", this.servcieName);
		return headers;

	}
	
	@Test
	public void findCellphonesByUserIdTest() {
		List<CellPhone> cellphones = ssoService.findCellphonesByUserId("fc1fd9e71de4f8c53bb7d2f1182772d3");
		Assert.assertNotNull(cellphones);
	}
	
	@Test
	public void findDisplayNameByUserIdTest() {
		String displayName = ssoService.findDisplayNameByUserId("fc1fd9e71de4f8c53bb7d2f1182772d3");
		Assert.assertTrue(displayName.equals("mervin2"));
	}

	@Test
	public void findDeviceNameByDeviceIdTest() {
		Device device = ssoService.findDeviceNameStatusByDeviceId("device20150227");
		Assert.assertTrue(device.getName().equals("deviceName"));
	}
}
