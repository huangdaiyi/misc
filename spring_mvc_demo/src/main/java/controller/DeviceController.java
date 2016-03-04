package controller;

import model.request.GetDeviceRequest;
import model.request.UpdateDeviceRequest;
import exception.MetadataException;

public interface DeviceController {

	public Object getDevice(GetDeviceRequest getDeviceRequest) throws MetadataException;

	public void updateDevice(UpdateDeviceRequest updateDeviceRequest) throws MetadataException;

	//public List<GetAllDeviceResponse> getAllDevice(GetAllDeviceRequest getAllDeviceRequest) throws MetadataException;
}