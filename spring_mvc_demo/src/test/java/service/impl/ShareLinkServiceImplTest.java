package service.impl;

import static org.junit.Assert.assertEquals;
import model.Metadata;
import model.PathInfo;
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

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import constants.HttpStatus;
import dao.MetadataDao;
import dao.ShareLinkDao;
import exception.MetadataException;
import factory.PathFactory;

public class ShareLinkServiceImplTest {
	@Mock
	private ShareLinkDao shareLinkDao;

	@Mock
	private MetadataDao metadataDao;

	@Mock
	private PathFactory pathFactory;

	@InjectMocks
	private ShareLinkServiceImpl impl;

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void addShareLinkBAD_REQUESTTest() {
		HttpStatus status = HttpStatus.OK;
		AddShareLinkRequest request = Mockito.mock(AddShareLinkRequest.class);
		try {
			impl.addShareLink(request);
		} catch (MetadataException ex) {
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);

		status = HttpStatus.OK;
		Mockito.when(request.getPath()).thenReturn("Hardy");
		try {
			impl.addShareLink(request);
		} catch (MetadataException ex) {
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}

	@Test
	public void addShareLinkTest() {
		AddShareLinkRequest request = Mockito.mock(AddShareLinkRequest.class);
		ShareLink shareLink = Mockito.mock(ShareLink.class);
		Mockito.when(shareLink.getLink()).thenReturn("test link");

		Mockito.when(request.getExpired()).thenReturn("2015-02-28");
		Mockito.when(request.getPath()).thenReturn("test path");
		PathInfo pathInfo = new PathInfo();
		Metadata metadata = new Metadata();
		Mockito.when(pathFactory.parsePathInfo(request,false, true)).thenReturn(pathInfo);
		Mockito.when(
				shareLinkDao.createShareLink(Matchers.any(ShareLink.class)))
				.thenReturn(shareLink);

		Mockito.when(
				metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
						pathInfo.getDeviceUid(), pathInfo.getFullSourcePath()))
				.thenReturn(metadata);

		AddShareLinkResponse response = impl.addShareLink(request);
		assertEquals(shareLink.getLink(), response.getLink());
	}

	@Test
	public void getShareLinkBAD_REQUESTTest() {
		HttpStatus status = HttpStatus.OK;
		GetShareLinkRequest request = Mockito.mock(GetShareLinkRequest.class);

		try {
			impl.getShareLink(request);
		} catch (MetadataException ex) {
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}

	@Test
	public void getShareLinkRootTest() {
		GetShareLinkRequest request = Mockito.mock(GetShareLinkRequest.class);
		Mockito.when(request.getLink()).thenReturn("test link");

		ShareLink shareLink = new ShareLink();
		shareLink.setMetadataIndexId("test id");
		shareLink.setExpired(100000L);
		shareLink.setLastEditTime(122121L);
		shareLink.setLink("test Link");

		Metadata metadata = Mockito.mock(Metadata.class);

		Mockito.when(metadata.getParentId()).thenReturn(null);
		Mockito.when(metadata.isFolder()).thenReturn(false);
		Mockito.when(metadata.getSourcePath()).thenReturn("");
		Mockito.when(metadata.getOwnerId()).thenReturn("hardy");
		Mockito.when(metadata.getOriginName()).thenReturn("hardy");

		Mockito.when(shareLinkDao.getShareLinkInfo(request.getLink()))
				.thenReturn(shareLink);
		Mockito.when(metadataDao.getMetadata(shareLink.getMetadataIndexId()))
				.thenReturn(metadata);

		GetShareLinkResponse response = impl.getShareLink(request);
		Assert.assertNotNull(response);
		Assert.assertEquals("", response.getPath());
	}

	@Test
	public void getShareLinkTest() {
		GetShareLinkRequest request = Mockito.mock(GetShareLinkRequest.class);
		Mockito.when(request.getLink()).thenReturn("test link");

		ShareLink shareLink = new ShareLink();
		shareLink.setMetadataIndexId("test id");
		shareLink.setExpired(100000L);
		shareLink.setLastEditTime(122121L);
		shareLink.setLink("test Link");

		Metadata metadata = Mockito.mock(Metadata.class);

		Mockito.when(metadata.getParentId()).thenReturn("not null");
		Mockito.when(metadata.isFolder()).thenReturn(false);
		Mockito.when(metadata.getSourcePath()).thenReturn("");
		Mockito.when(metadata.getOwnerId()).thenReturn("hardy");
		Mockito.when(metadata.getOriginName()).thenReturn("hardy");

		Mockito.when(shareLinkDao.getShareLinkInfo(request.getLink()))
				.thenReturn(shareLink);
		Mockito.when(metadataDao.getMetadata(shareLink.getMetadataIndexId()))
				.thenReturn(metadata);

		Mockito.when(metadataDao.getFullPath(metadata.getParentId()))
				.thenReturn("userid/test");

		GetShareLinkResponse response = impl.getShareLink(request);
		Assert.assertNotNull(response);
		Assert.assertEquals("test/" + metadata.getOriginName(),
				response.getPath());
	}

	@Test
	public void existsShareLinkBAD_REQUESTTest() {
		HttpStatus status = HttpStatus.OK;
		ExistsShareLinkRequest request = Mockito
				.mock(ExistsShareLinkRequest.class);
		Mockito.when(request.getPath()).thenReturn("");

		try {
			
			impl.existsShareLink(request);
		} catch (MetadataException ex) {
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		try {
			impl.existsShareLink(null);
		} catch (MetadataException ex) {
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);

	}

	@Test
	public void existsShareLinkTest() {

		ExistsShareLinkRequest request = Mockito
				.mock(ExistsShareLinkRequest.class);
		ShareLink shareLink = Mockito.mock(ShareLink.class);
		PathInfo pathInfo = Mockito.mock(PathInfo.class);
		Metadata metadata = new Metadata();
		metadata.setId("test id");

		Mockito.when(request.getPath()).thenReturn("test path");
		Mockito.when(shareLink.getLink()).thenReturn("test link");
		Mockito.when(pathInfo.getFullOwnerPath()).thenReturn("hardy/test");
		Mockito.when(pathInfo.getDeviceUid()).thenReturn("test deviceuid");
		Mockito.when(pathInfo.getFullSourcePath()).thenReturn(
				"test full source path");

		Mockito.when(shareLinkDao.getShareLink(metadata.getId())).thenReturn(
				"test link");
		Mockito.when(pathFactory.parsePathInfo(request,false, true)).thenReturn(pathInfo);

		Mockito.when(
				metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
						pathInfo.getDeviceUid(), pathInfo.getFullSourcePath()))
				.thenReturn(metadata);

		ExistsShareLinkResponse response = impl.existsShareLink(request);
		assertEquals(shareLink.getLink(), response.getLink());
	}

	@Test
	public void confirmShareLinkBAD_REQUESTTest() {
		HttpStatus status = HttpStatus.OK;
		ConfirmShareLinkRequest request = Mockito
				.mock(ConfirmShareLinkRequest.class);

		try {
			impl.confirmShareLink(request);
		} catch (MetadataException ex) {
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}

	@Test
	public void confirmShareLinkErrorTest() {
		ConfirmShareLinkRequest request = Mockito
				.mock(ConfirmShareLinkRequest.class);
		Mockito.when(request.getLink()).thenReturn("hardy");
		Mockito.when(request.getPassword()).thenReturn("123");

		ShareLink shareLink = new ShareLink();
		shareLink.setPassword("123123");

		Mockito.when(shareLinkDao.getShareLinkInfo(request.getLink()))
				.thenReturn(shareLink);
		ConfirmShareLinkResponse response = impl.confirmShareLink(request);
		assertEquals(false, response.isResult());
	}

	@Test
	public void confirmShareLinkRightTest() {
		ConfirmShareLinkRequest request = Mockito
				.mock(ConfirmShareLinkRequest.class);
		Mockito.when(request.getLink()).thenReturn("hardy");
		Mockito.when(request.getPassword()).thenReturn("123");

		ShareLink shareLink = new ShareLink();
		shareLink.setPassword("123");

		Mockito.when(shareLinkDao.getShareLinkInfo(request.getLink()))
				.thenReturn(shareLink);
		ConfirmShareLinkResponse response = impl.confirmShareLink(request);
		assertEquals(true, response.isResult());
	}

	@Test
	public void deleteShareLinkBAD_REQUESTTest() {
		HttpStatus status = HttpStatus.OK;
		DeleteShareLinkRequest request = Mockito
				.mock(DeleteShareLinkRequest.class);

		try {
			impl.deleteShareLink(request);
		} catch (MetadataException ex) {
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}

	@Test
	public void deleteShareLinkTest() {
		DeleteShareLinkRequest request = Mockito
				.mock(DeleteShareLinkRequest.class);
		Mockito.when(request.getPath()).thenReturn("hardy");
		Mockito.when(request.getSourcePath()).thenReturn("hardy");
		PathInfo pathInfo = Mockito.mock(PathInfo.class);
		Mockito.when(pathInfo.getFullOwnerPath()).thenReturn("hardy/test");
		Mockito.when(pathInfo.getDeviceUid()).thenReturn("test deviceuid");
		Mockito.when(pathInfo.getFullSourcePath()).thenReturn(
				"test full source path");
		Metadata metadata = new Metadata();

		Mockito.when(pathFactory.parsePathInfo(request,false, true)).thenReturn(pathInfo);

		Mockito.when(
				metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
						pathInfo.getDeviceUid(), pathInfo.getFullSourcePath()))
				.thenReturn(metadata);

		impl.deleteShareLink(request);
	}

	@Test
	public void updateShareLinkBAD_REQUESTTest() {
		HttpStatus status = HttpStatus.OK;
		UpdateShareLinkRequest request = Mockito
				.mock(UpdateShareLinkRequest.class);

		try {
			impl.updateShareLink(request);
		} catch (MetadataException ex) {
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}

	@Test
	public void updateShareLinkTest() {
		UpdateShareLinkRequest request = Mockito
				.mock(UpdateShareLinkRequest.class);
		Mockito.when(request.getPath()).thenReturn("hardy");
		ShareLink shareLink = Mockito.mock(ShareLink.class);
		PathInfo pathInfo = Mockito.mock(PathInfo.class);
		
		Mockito.when(pathFactory.parsePathInfo(request,false, true)).thenReturn(pathInfo);
		Mockito.when(
				shareLinkDao.createShareLink(Matchers.any(ShareLink.class)))
				.thenReturn(shareLink);
		Mockito.when(shareLink.getLink()).thenReturn("test link");

		Mockito.when(shareLinkDao.updateShareLink(Matchers.any(ShareLink.class))).thenReturn(
				shareLink);
	

		Mockito.when(request.getExpired()).thenReturn("2015-02-28");
		Mockito.when(pathInfo.getFullOwnerPath()).thenReturn("hardy/test");
		Mockito.when(pathInfo.getDeviceUid()).thenReturn("test deviceuid");
		Mockito.when(pathInfo.getFullSourcePath()).thenReturn(
				"test full source path");
		
		Metadata metadata = new Metadata();
		Mockito.when(
				metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
						pathInfo.getDeviceUid(), pathInfo.getFullSourcePath()))
				.thenReturn(metadata);

		boolean result = impl.updateShareLink(request);
		Assert.assertTrue(result);
	}

}
