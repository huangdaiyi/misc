package service.impl;

import model.BaseMetadata;
import model.PathInfo;
import model.PathRequestBase;
import model.ShareLink;
import model.request.AddShareLinkRequest;
import model.request.ConfirmShareLinkRequest;
import model.request.DeleteShareLinkRequest;
import model.request.ExistsShareLinkRequest;
import model.request.GetShareLinkRequest;
import model.request.UpdateShareLinkRequest;
import model.response.AddShareLinkResponse;
import model.response.ConfirmShareLinkResponse;
import model.response.ExistsShareLinkResponse;
import model.response.GetShareLinkResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import service.ShareLinkService;
import utils.DateUtils;
import utils.StringUtils;
import constants.CommonFolders;
import constants.HttpStatus;
import dao.ShareLinkDao;
import exception.MetadataException;
import factory.MetadataFactory;
import factory.PathFactory;

@Service
public class ShareLinkServiceImpl implements ShareLinkService {

	@Autowired
	private ShareLinkDao shareLinkDao;
	@Autowired
	private PathFactory pathFactory;
	@Autowired
	MetadataFactory metadataFactory;

	@Override
	public AddShareLinkResponse addShareLink(AddShareLinkRequest request)
			throws MetadataException {
		if (null == request) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (request.getPath().toLowerCase().equals("my shared folders")){
			throw new MetadataException(HttpStatus.CANT_LINK_FOLDER);
		}
		ShareLink shareLink = shareLinkDao
				.createShareLink(requestToShareLink(request));

		AddShareLinkResponse response = new AddShareLinkResponse();
		response.setLink(shareLink.getLink());
		return response;
	}

	@Override
	public GetShareLinkResponse getShareLink(
			GetShareLinkRequest getShareLinkRequest) throws MetadataException {
		if (null == getShareLinkRequest
				|| StringUtils.isNullOrEmpty(getShareLinkRequest.getLink())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		ShareLink shareLink = shareLinkDao.getShareLinkInfo(getShareLinkRequest
				.getLink());

		if (null == shareLink || StringUtils.isNullOrEmpty(shareLink.getLink())) {
			throw new MetadataException(HttpStatus.SHARE_LINK_NOT_EXIST);
		}
		
		BaseMetadata metadata = metadataFactory.getBaseMetadataById(shareLink.getMetadataIndexId(), shareLink.isBackup());
		
		if (metadata == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}

		String originalPath = metadataFactory.getMetadataOriginalPath(metadata);

		return shareLink.toGetShareLinkResponse(metadata, originalPath,
				getShareLinkRequest.isDetail());
	}

	@Override
	public ExistsShareLinkResponse existsShareLink(
			ExistsShareLinkRequest existsShareLinkRequest)
			throws MetadataException {

		BaseMetadata metadata = getMetadata(existsShareLinkRequest, existsShareLinkRequest.isFolder());
		String link = shareLinkDao.getShareLink(metadata.getId());
		ExistsShareLinkResponse response = new ExistsShareLinkResponse();
		response.setLink(link == null ? "" : link);
		return response;
	}

	@Override
	public ConfirmShareLinkResponse confirmShareLink(
			ConfirmShareLinkRequest cnfirmShareLinkRequest)
			throws MetadataException {

		if (null == cnfirmShareLinkRequest
				|| StringUtils.isNullOrEmpty(cnfirmShareLinkRequest.getLink())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		ShareLink shareLink = shareLinkDao
				.getShareLinkInfo(cnfirmShareLinkRequest.getLink());

		if (null == shareLink) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}

		return new ConfirmShareLinkResponse(cnfirmShareLinkRequest
				.getPassword().equals(shareLink.getPassword()));
	}

	@Override
	public void deleteShareLink(DeleteShareLinkRequest deleteShareLinkRequest)
			throws MetadataException {
		BaseMetadata metadata = getMetadata(deleteShareLinkRequest, deleteShareLinkRequest.isFolder());
		shareLinkDao.deleteShareLink(metadata.getId());
	}

	@Override
	public Boolean updateShareLink(UpdateShareLinkRequest updateShareLinkRequest)
			throws MetadataException {

		if (null == updateShareLinkRequest) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		Long timestamp = DateUtils.convertDatetime(updateShareLinkRequest
				.getExpired());

		BaseMetadata metadata = getMetadata(updateShareLinkRequest, updateShareLinkRequest.isFolder());
		ShareLink shareLink = new ShareLink();
		shareLink.setMetadataIndexId(metadata.getId());
		shareLink.setBackup(false);
		shareLink.setExpired(timestamp);
		shareLink.setLastEditTime(DateUtils.nowUTCTimestamp());
		shareLink.setLastEditUser(updateShareLinkRequest.getUserId());
		shareLink.setPassword(updateShareLinkRequest.getPassword());
		return shareLinkDao.updateShareLink(shareLink) != null;
	}

	private ShareLink requestToShareLink(AddShareLinkRequest request) {
		ShareLink shareLink = new ShareLink();

		Long timestamp = DateUtils.convertDatetime(request.getExpired());
		BaseMetadata metadata = getMetadata(request, request.isFolder());

		shareLink.setMetadataIndexId(metadata.getId());
		// TODO
		shareLink.setBackup(false);
		String lowerCaseOwnerPath = request.getPath().toLowerCase();
		if (lowerCaseOwnerPath.equals(CommonFolders.MY_BACKUP_DATA.toString()) || lowerCaseOwnerPath.startsWith(CommonFolders.MY_BACKUP_DATA.toString().concat("/"))){
			shareLink.setBackup(true);
		}
		shareLink.setFolder(request.isFolder());
		shareLink.setExpired(timestamp);
		shareLink.setLastEditTime(DateUtils.nowUTCTimestamp());
		shareLink.setLastEditUser(request.getUserId());
		shareLink.setLink(StringUtils.getCrc32(metadata.getId()));
		shareLink.setToken(request.getToken());
		shareLink.setUser(request.getUserId());
		shareLink.setPassword(request.getPassword());
		return shareLink;
	}

	private BaseMetadata getMetadata(PathRequestBase request, boolean isFolder) {
		if (null == request || StringUtils.isNullOrEmpty(request.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		
		PathInfo pathInfo = pathFactory.parsePathInfo(request,isFolder, true);
		metadataFactory.getBaseMetadataByPath(pathInfo);
		//metadataDao.get
		BaseMetadata metadata = metadataFactory.getBaseMetadataByPath(pathInfo);
		//metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
	    //BaseMetadata metadata = isFolder ? metadataFactory.getFolderByPathInfo(pathInfo) : metadataFactory.getFileByPathInfo(pathInfo);

		if (metadata == null || metadata.isFolder() != isFolder) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}

		return metadata;
	}
}
