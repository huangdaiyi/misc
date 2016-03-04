package service;

import model.request.GetUserRequest;
import model.request.RestCommonRequest;
import model.request.RestUserRequest;
import model.request.UpdateSettingsRequest;
import model.request.UpdateUserDiskSpaceRequest;
import model.response.GetUserResponse;
import exception.MetadataException;

public interface UserService {

	public GetUserResponse getUser(GetUserRequest getUserRequest)
			throws MetadataException;

	public boolean reset(RestUserRequest restUserRequest)
			throws MetadataException;

	public boolean resetCommon(RestCommonRequest restCommonRequest)
			throws MetadataException;

	public boolean updateSettings(UpdateSettingsRequest updateSettingsRequest)
			throws MetadataException;

	public void updateUserDiskSpace(UpdateUserDiskSpaceRequest updateUserDiskSpaceRequest) throws MetadataException;
}
