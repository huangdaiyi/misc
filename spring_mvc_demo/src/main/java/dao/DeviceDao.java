package dao;

import java.util.List;

import model.Device;
import model.MobileFolders;

public interface DeviceDao {

	public void update(String userId, String deviceUid, String settings);

	public Device getDevice(String userId, String deviceUid);

	public List<Device> getDevices(String user);

	public Device createDevice(Device device);

	public void deleteDevice(int id);

	public MobileFolders getMobileFolders(String userId, String fullSourcePath);

	public void createMobileFolders(String userId, String fullSourcePath, String deviceId);

	public Device getDeviceByDeviceId(String deviceId, String userId);
}