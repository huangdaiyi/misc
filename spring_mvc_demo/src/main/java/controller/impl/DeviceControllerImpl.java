package controller.impl;

import model.request.GetDeviceRequest;
import model.request.UpdateDeviceRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import service.DeviceService;
import utils.StringUtils;
import annotation.Readonly;
import constants.HttpStatus;
import controller.DeviceController;
import exception.MetadataException;

@RestController
public class DeviceControllerImpl implements DeviceController {

	@Autowired
	private DeviceService deviceService;

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/device/info", method = RequestMethod.POST)
	public Object getDevice(@RequestBody GetDeviceRequest getDeviceRequest) throws MetadataException {
		if (StringUtils.isNullOrEmpty(getDeviceRequest.getDeviceUid())) {
			return deviceService.getAllDevice(getDeviceRequest);
		}
		return deviceService.getDevice(getDeviceRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/device/update", method = RequestMethod.POST)
	public void updateDevice(@RequestBody UpdateDeviceRequest updateDeviceRequest) throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateDeviceRequest.getDeviceUid())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		deviceService.updateDevice(updateDeviceRequest);
	}

}
