package service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import model.Device;
import model.request.GetDeviceRequest;
import model.request.UpdateDeviceRequest;
import model.response.GetDeviceResponse;
import model.response.SSODevicesInfo;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import service.DeviceService;
import service.SSOService;
import base.BaseTestCase;
import constants.HttpStatus;
import dao.DeviceDao;
import exception.MetadataException;

@ContextConfiguration("device.xml")
public class DeviceServiceImplTest extends BaseTestCase {

	@InjectMocks
	@Autowired
	private DeviceService deviceService;

	@Mock
	private SSOService ssoService;

	@Mock
	private DeviceDao deviceDao;

	@Autowired
	private GetDeviceRequest getDeviceRequest;

	@Autowired
	private Device deviceToget;

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testGetDeviceMock() {
		GetDeviceRequest getDeviceRequest = Mockito
				.mock(GetDeviceRequest.class);
		Mockito.when(
				deviceDao.getDevice(getDeviceRequest.getUserId(), getDeviceRequest.getDeviceUid())).thenReturn(deviceToget);
		Mockito.when(ssoService.getDevicesByToken(getDeviceRequest))
				.thenReturn(buildSSODevices());
		GetDeviceResponse response = deviceService.getDevice(getDeviceRequest);
		Assert.assertNotNull(response);
	}

	@Test
	public void testGetDeviceBad_RequestBySSOMock() {
		HttpStatus statusCode = null;
		GetDeviceRequest getDeviceRequest = Mockito
				.mock(GetDeviceRequest.class);
		Mockito.when(
				deviceDao.getDevice(getDeviceRequest.getUserId(), getDeviceRequest.getDeviceUid())).thenReturn(deviceToget);
		Mockito.when(ssoService.getDevicesByToken(getDeviceRequest))
				.thenReturn(null);
		try {
			deviceService.getDevice(getDeviceRequest);
		} catch (MetadataException e) {
			statusCode = e.getHttpStatus();
		}
		Assert.assertEquals(500, statusCode);
	}

	@Test
	public void testGetDeviceBad_RequestByDeviceDaoMock() {
		HttpStatus statusCode = null;
		GetDeviceRequest getDeviceRequest = Mockito
				.mock(GetDeviceRequest.class);
		Mockito.when(
				deviceDao.getDevice(getDeviceRequest.getUserId(), getDeviceRequest.getDeviceUid())).thenReturn(null);
		Mockito.when(ssoService.getDevicesByToken(getDeviceRequest))
				.thenReturn(buildSSODevices());
		try {
			deviceService.getDevice(getDeviceRequest);
		} catch (MetadataException e) {
			statusCode = e.getHttpStatus();
		}
		Assert.assertEquals(404, statusCode);
	}

	@Test
	public void testUpdateSettingBad_Request() {
		HttpStatus statusCode = null;
		UpdateDeviceRequest updateDeviceRequest = Mockito
				.mock(UpdateDeviceRequest.class);
		Mockito.when(
				deviceDao.getDevice(updateDeviceRequest.getUserId(), updateDeviceRequest.getDeviceUid())).thenReturn(null);
		try {
			deviceService.updateDevice(updateDeviceRequest);
		} catch (MetadataException e) {
			statusCode = e.getHttpStatus();
		}
		Assert.assertEquals(HttpStatus.DEVICE_NOT_EXIST, statusCode);
	}

	@Test
	public void testUpdateSetting() {
		UpdateDeviceRequest updateDeviceRequest = Mockito
				.mock(UpdateDeviceRequest.class);
		Mockito.when(
				deviceDao.getDevice(updateDeviceRequest.getUserId(), updateDeviceRequest.getDeviceUid())).thenReturn(
				deviceToget);
		Boolean boolean1 = deviceService.updateDevice(updateDeviceRequest);
		Assert.assertTrue(boolean1);
	}

	private SSODevicesInfo buildSSODevices() {
		SSODevicesInfo ssoDevicesInfo = new SSODevicesInfo();
		Map<String, String> device = new HashMap<String, String>();
		device.put("device_status", "Login");
		device.put("device_id", "DEVICE1");
		device.put("device_name", "DEVICE1");
		Map<String, String> device2 = new HashMap<String, String>();
		device.put("device_status", "Login");
		device.put("device_id", "DEVICE2");
		device.put("device_name", "DEVICE2");
		List<Map<String, String>> devices = new ArrayList<Map<String, String>>();
		devices.add(device);
		devices.add(device2);
		ssoDevicesInfo.setDevices(devices);
		ssoDevicesInfo.setStatus("sueecss");
		return ssoDevicesInfo;
	}
}
