package service;


import model.RequestBase;
import model.request.GetDeviceRequest;
import model.request.UpdateDeviceRequest;
import model.response.GetAllDeviceResponse;
import model.response.GetDeviceResponse;
import exception.MetadataException;

public interface DeviceService {
	public GetDeviceResponse createDevice(RequestBase requestBase)throws MetadataException;

	public GetDeviceResponse getDevice(GetDeviceRequest getDeviceRequest)throws MetadataException;
	
	public Boolean updateDevice(UpdateDeviceRequest updateDeviceRequest) throws MetadataException;
	
	public GetAllDeviceResponse getAllDevice(RequestBase getAllDeviceRequest)throws MetadataException;

	public void unregister(String userId, String deviceId) throws MetadataException;
}