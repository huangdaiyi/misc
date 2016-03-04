package controller.impl;

import model.request.AddShareLinkRequest;
import model.request.ConfirmShareLinkRequest;
import model.request.DeleteShareLinkRequest;
import model.request.ExistsShareLinkRequest;
import model.request.GetClientVersionRequest;
import model.request.GetShareLinkRequest;
import model.request.SendAllMetadataToMQRequest;
import model.request.UpdateClientVersionRequest;
import model.request.UpdateShareLinkRequest;
import model.request.UpdateUserDiskSpaceRequest;
import model.response.AddShareLinkResponse;
import model.response.ClientVersionResponse;
import model.response.ConfirmShareLinkResponse;
import model.response.ExistsShareLinkResponse;
import model.response.GetShareLinkResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import service.AdminService;
import service.ShareLinkService;
import service.UserService;
import utils.StringUtils;
import annotation.AdminAuthorizationOnly;
import annotation.AllowAdminAuthorization;
import annotation.Readonly;
import constants.HttpStatus;
import controller.AdminController;
import exception.MetadataException;

@RestController
public class AdminControllerImpl implements AdminController {

	@Autowired
	private AdminService adminService;
	@Autowired
	private ShareLinkService shareLinkService;
	@Autowired
	private UserService userService;

	@Override
	@RequestMapping(value = "/api/v1/admin/addsharelink", method = RequestMethod.POST)
	public AddShareLinkResponse addShareLink(
			@RequestBody AddShareLinkRequest addShareLinkRequest)
			throws MetadataException {

		if (StringUtils.isNullOrEmpty(addShareLinkRequest.getPath())
				|| addShareLinkRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		return shareLinkService.addShareLink(addShareLinkRequest);
	}

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/admin/getsharelink", method = RequestMethod.POST)
	public GetShareLinkResponse getShareLink(
			@RequestBody GetShareLinkRequest getShareLinkRequest)
			throws MetadataException {

		if (StringUtils.isNullOrEmpty(getShareLinkRequest.getLink())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		return shareLinkService.getShareLink(getShareLinkRequest);
	}

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/admin/existssharelink", method = RequestMethod.POST)
	public ExistsShareLinkResponse existsShareLink(
			@RequestBody ExistsShareLinkRequest existsShareLinkRequest)
			throws MetadataException {

		if (StringUtils.isNullOrEmpty(existsShareLinkRequest.getPath())
				|| existsShareLinkRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		return shareLinkService.existsShareLink(existsShareLinkRequest);
	}

	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/admin/confirmsharelink", method = RequestMethod.POST)
	public ConfirmShareLinkResponse confirmShareLink(
			@RequestBody ConfirmShareLinkRequest cnfirmShareLinkRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(cnfirmShareLinkRequest.getLink())
				|| cnfirmShareLinkRequest.getPassword() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		return shareLinkService.confirmShareLink(cnfirmShareLinkRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/admin/deletesharelink", method = RequestMethod.POST)
	public void deleteShareLink(
			@RequestBody DeleteShareLinkRequest deleteShareLinkRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(deleteShareLinkRequest.getPath())
				|| deleteShareLinkRequest.getSourcePath() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		shareLinkService.deleteShareLink(deleteShareLinkRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/admin/updatesharelink", method = RequestMethod.POST)
	public void updateShareLink(
			@RequestBody UpdateShareLinkRequest updateShareLinkRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateShareLinkRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		shareLinkService.updateShareLink(updateShareLinkRequest);
	}

	@Override
	@AdminAuthorizationOnly
	@RequestMapping(value = "/api/v1/admin/send_all_metadata_to_mq", method = RequestMethod.POST)
	public void sendAllMetadataToMQ(
			@RequestBody SendAllMetadataToMQRequest sendAllMetadataToMQRequest)
			throws MetadataException {
		adminService.sendAllMetadataToMQ(sendAllMetadataToMQRequest);
	}
	
	@Override
	@Readonly
	@AllowAdminAuthorization
	@RequestMapping(value = "/api/v1/admin/version", method = RequestMethod.POST)
	public ClientVersionResponse getClientVersion(
			@RequestBody GetClientVersionRequest getClientVersionRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(getClientVersionRequest.getPlatform())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return adminService.getClientVersion(getClientVersionRequest);
	}
	
	@Override
	@AdminAuthorizationOnly
	@RequestMapping(value = "/api/v1/admin/update_version", method = RequestMethod.POST)
	public void updateClientVersion(
			@RequestBody UpdateClientVersionRequest updateClientVersionRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateClientVersionRequest.getPlatform())
				|| StringUtils.isNullOrEmpty(updateClientVersionRequest.getVersion())
				|| StringUtils.isNullOrEmpty(updateClientVersionRequest.getUpdateType())
				|| StringUtils.isNullOrEmpty(updateClientVersionRequest.getDownloadUrl())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		adminService.updateClientVersion(updateClientVersionRequest);
	}

	@Override
	@AdminAuthorizationOnly
	@RequestMapping(value = "/api/v1/admin/update_user_disk_space", method = RequestMethod.POST)
	public void updateUserDiskSpace(@RequestBody UpdateUserDiskSpaceRequest updateUserDiskSpaceRequest) throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateUserDiskSpaceRequest.getAccessKey()) || updateUserDiskSpaceRequest.getTotal() == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		userService.updateUserDiskSpace(updateUserDiskSpaceRequest);
	}

}
