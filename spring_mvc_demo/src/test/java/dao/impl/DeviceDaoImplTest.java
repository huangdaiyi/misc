package dao.impl;


import static org.junit.Assert.assertNotNull;
import model.Device;
import model.request.GetDeviceRequest;
import model.request.UpdateDeviceRequest;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import base.BaseTestCase;
import dao.DeviceDao;

@ContextConfiguration("device.xml")
public class DeviceDaoImplTest extends BaseTestCase{
	@Autowired
	private DeviceDao deviceDao;
	@Autowired
	private GetDeviceRequest getDeviceRequest;
	@Autowired
	private UpdateDeviceRequest updateDeviceRequest;
	
	@Test
	public void testGetDeviceDao(){
		Device device = deviceDao.getDevice(getDeviceRequest.getUserId(), getDeviceRequest.getDeviceUid());
		assertNotNull(device);

	}
	@Test
	public void testUpdateDeviceDao(){
		deviceDao.update(updateDeviceRequest.getUserId(), updateDeviceRequest.getDeviceUid(), updateDeviceRequest.getSettings());
	}

}
