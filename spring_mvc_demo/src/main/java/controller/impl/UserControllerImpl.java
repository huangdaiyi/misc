package controller.impl;

import model.request.GetUserRequest;
import model.request.RestCommonRequest;
import model.request.RestUserRequest;
import model.request.UpdateSettingsRequest;
import model.response.GetUserResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import service.UserService;
import annotation.Readonly;
import constants.HttpStatus;
import controller.UserController;
import exception.MetadataException;

@RestController
public class UserControllerImpl implements UserController {

	@Autowired
	private UserService userService;

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/user/info", method = RequestMethod.POST)
	public GetUserResponse getUser(GetUserRequest getUserRequest) {
		return userService.getUser(getUserRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/user/reset", method = RequestMethod.POST)
	public void reset(@RequestBody RestUserRequest restUserRequest)
			throws MetadataException {
		userService.reset(restUserRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/user/resetcommon", method = RequestMethod.POST)
	public void resetCommon(@RequestBody RestCommonRequest restCommonRequest)
			throws MetadataException {
		if (restCommonRequest.getPath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		userService.resetCommon(restCommonRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/user/settings", method = RequestMethod.POST)
	public void updateSettings(
			@RequestBody UpdateSettingsRequest updateSettingsRequest)
			throws MetadataException {
		userService.updateSettings(updateSettingsRequest);

	}
}
