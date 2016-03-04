package controller.impl;

import java.util.List;

import model.RequestBase;
import model.request.AddGlobalAppRequest;
import model.request.DeleteGlobalAppRequest;
import model.request.DeleteUserAppRequest;
import model.request.GetActivityHistoryRequest;
import model.request.GetUserAppRequest;
import model.request.SwitchAppSyncableRequest;
import model.request.UpdateUserAppRequest;
import model.response.GetActivityHistoryResponse;
import model.response.GetGlobalAppResponse;
import model.response.GetUserAppResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import service.SyncService;
import utils.StringUtils;
import annotation.Readonly;
import constants.HttpStatus;
import controller.SyncController;
import exception.MetadataException;

@RestController
public class SyncControllerImpl implements SyncController {

	@Autowired
	private SyncService syncService;

	@Override
	@RequestMapping(value = "/api/v1/sync/add_global_app", method = RequestMethod.POST)
	public void addGlobalApp(
			@RequestBody AddGlobalAppRequest addGlobalAppRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(addGlobalAppRequest.getAppName())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		syncService.addGlobalApp(addGlobalAppRequest);
	}
	
	@Override
	@RequestMapping(value = "/api/v1/sync/delete_global_app", method = RequestMethod.POST)
	public void deleteGlobalApp(
			@RequestBody DeleteGlobalAppRequest deleteGlobalAppRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(deleteGlobalAppRequest.getAppName())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		syncService.deleteGlobalApp(deleteGlobalAppRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/sync/get_global_app", method = RequestMethod.POST)
	public List<GetGlobalAppResponse> getGlobalApp(
			@RequestBody RequestBase getGlobalAppRequest)
			throws MetadataException {
		return syncService.getGlobalApp(getGlobalAppRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/sync/set_user_app", method = RequestMethod.POST)
	public void setUserApp(
			@RequestBody UpdateUserAppRequest updateUserAppRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateUserAppRequest.getDeviceUniqueId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		syncService.updateUserApp(updateUserAppRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/sync/get_user_app", method = RequestMethod.POST)
	public List<GetUserAppResponse> getUserApp(
			@RequestBody GetUserAppRequest getUserAppRequest)
			throws MetadataException {
		return syncService.getUserApp(getUserAppRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/sync/delete_user_app", method = RequestMethod.POST)
	public void deleteUserApp(
			@RequestBody DeleteUserAppRequest deleteUserAppRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(deleteUserAppRequest.getDeviceUniqueId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (StringUtils.isNullOrEmpty(deleteUserAppRequest.getAppName())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		syncService.deleteUserApp(deleteUserAppRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/sync/switch_app_syncable", method = RequestMethod.POST)
	public void switchAppSyncable(
			@RequestBody SwitchAppSyncableRequest switchAppSyncableRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(switchAppSyncableRequest.getDeviceUniqueId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (StringUtils.isNullOrEmpty(switchAppSyncableRequest.getAppName())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		syncService.switchAppSyncable(switchAppSyncableRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/sync/search_history", method = RequestMethod.POST)
	public GetActivityHistoryResponse searchHistory(
			@RequestBody GetActivityHistoryRequest getActivityHistoryRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(getActivityHistoryRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return syncService.searchHistory(getActivityHistoryRequest);
	}

}
