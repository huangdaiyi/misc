package controller;

import model.request.GetUserRequest;
import model.request.RestCommonRequest;
import model.request.RestUserRequest;
import model.request.UpdateSettingsRequest;
import model.response.GetUserResponse;
import exception.MetadataException;

public interface UserController {

	public GetUserResponse getUser(GetUserRequest getUserRequest)
			throws MetadataException;

	public void reset(RestUserRequest restUserRequest) throws MetadataException;

	public void resetCommon(RestCommonRequest restCommonRequest)
			throws MetadataException;

	public void updateSettings(UpdateSettingsRequest updateSettingsRequest)
			throws MetadataException;
}
