package service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import model.CellPhone;
import model.CollaborateMember;
import model.Metadata;
import model.PathInfo;
import model.request.CreateCollaborateRequest;
import model.response.CreateCollaborateResponse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import service.FolderService;
import service.SSOService;
import base.BaseTestCase;
import constants.MetadataType;
import dao.CollaborateDao;
import dao.MetadataDao;
import exception.MetadataException;
import factory.MetadataFactory;
import factory.PathFactory;

@ContextConfiguration("collaborate.xml")
public class CollaborateServiceImplTest extends BaseTestCase {
	@InjectMocks
	private CollaborateServiceImpl collaborateService;

	@Mock
	private FolderService folderService;

	@Mock
	private MetadataDao metadataDao;

	@Mock
	private CollaborateDao collaborateDao;

	@Mock
	private PathFactory pathFactory;

	@Mock
	private SSOService ssoService;

	@Mock
	private MetadataFactory metadataFactory;

	// @Autowired
	// private CreateCollaborateRequest createCollaborateRequest;

	@Autowired
	private Metadata originMetadata;

	@Autowired
	private Metadata normalMetadata;

	@Autowired
	private Metadata collaborateFolder;
	@Autowired
	private CreateCollaborateRequest transformCollaborateRequest;
	@Autowired
	private Metadata parentMetadata;
	@Autowired
	private Metadata subMetadata;
	@Autowired
	private CollaborateMember collaborateMember;

	@Autowired
	private CellPhone cellPhones;

	@Autowired
	private PathInfo pathInfo;

	private String ownerId = "testOwnerId";

	private String userId = "testUserId";

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testCanAccessCollaborate4Memeber() {
		String sharedRootId = "sharedRootId";

		Mockito.when(metadataDao.getMetadata(sharedRootId)).thenReturn(
				collaborateFolder);
		List<CollaborateMember> members = Arrays.asList(collaborateMember);
		List<CellPhone> userCellPhones = Arrays.asList(cellPhones);
		Mockito.when(
				collaborateDao
						.getCollaborateMembersByMetadataIndexId(collaborateFolder
								.getSharedRootId())).thenReturn(members);
		Mockito.when(ssoService.findCellphonesByUserId(userId)).thenReturn(
				userCellPhones);
		boolean canAccess = collaborateService.canAccessCollaborate(
				sharedRootId, ownerId, userId,"");
		Assert.assertTrue(canAccess);
	}

	@Test
	public void testCanAccessCollaborate4Owner() {
		String sharedRootId = "sharedRootId";
		Mockito.when(metadataDao.getMetadata(sharedRootId)).thenReturn(
				collaborateFolder);

		boolean canAccess = collaborateService.canAccessCollaborate(
				sharedRootId, ownerId, ownerId,"");
		Assert.assertTrue(canAccess);
	}

	@Test
	public void testCanAccessCollaborate4fail() {
		String sharedRootId = "sharedRootId";

		Mockito.when(metadataDao.getMetadata(sharedRootId)).thenReturn(
				collaborateFolder);
		List<CollaborateMember> members = Arrays.asList();
		List<CellPhone> userCellPhones = Arrays.asList();
		Mockito.when(
				collaborateDao
						.getCollaborateMembersByMetadataIndexId(collaborateFolder
								.getSharedRootId())).thenReturn(members);
		Mockito.when(ssoService.findCellphonesByUserId(userId)).thenReturn(
				userCellPhones);
		try {
			collaborateService.canAccessCollaborate(sharedRootId, "test",
					userId,"");
		} catch (MetadataException e) {
			Assert.assertEquals(401, e.getHttpStatus().getCode());

		}
	}

	@Test
	public void testTransform() {
		Mockito.when(pathFactory.parsePathInfo(transformCollaborateRequest, false, true))
				.thenReturn(pathInfo);
		Mockito.when(
				metadataDao.getMetadataByPath(pathInfo.getFullOriginPath(), "",
						"")).thenReturn(originMetadata);
		List<Metadata> upperMetadatas = new ArrayList<Metadata>();
		upperMetadatas.add(parentMetadata);
		List<Metadata> subMetadatas = new ArrayList<Metadata>();
		subMetadatas.add(originMetadata);
		Mockito.when(metadataFactory.getAllUpperLevelMetadatas(originMetadata))
				.thenReturn(upperMetadatas);
		Mockito.doNothing()
				.when(collaborateDao)
				.createCollaborate(originMetadata.getId());
		Mockito.doNothing().when(metadataDao).batchUpdateMetadata(subMetadatas);
		Mockito.doNothing().when(metadataDao)
				.updateMetadataType(originMetadata.getId(), MetadataType.SHARE);

		Mockito.when(
				ssoService.findDisplayNameByUserId(transformCollaborateRequest
						.getUserId())).thenReturn("displayname");

		CreateCollaborateResponse response = collaborateService
				.transformToShared(transformCollaborateRequest);
		Assert.assertEquals(response.getName(), originMetadata.getName());
	}

	@Test
	public void testTransformFail() {
		Mockito.when(pathFactory.parsePathInfo(transformCollaborateRequest,false, true))
				.thenReturn(pathInfo);
		Mockito.when(
				metadataDao.getMetadataByPath(pathInfo.getFullOriginPath(), "",
						"")).thenReturn(originMetadata);
		List<Metadata> upperMetadatas = new ArrayList<Metadata>();
		upperMetadatas.add(parentMetadata);
		List<Metadata> subMetadatas = new ArrayList<Metadata>();
		subMetadatas.add(originMetadata);
		subMetadatas.add(subMetadata);
		Mockito.when(metadataFactory.getAllUpperLevelMetadatas(originMetadata))
				.thenReturn(upperMetadatas);
		Mockito.when(metadataFactory.getAllSubFoldersAndFiles(originMetadata))
				.thenReturn(subMetadatas);
		try {
			// if sub folder type != normal,throw MetadataException
			collaborateService.transformToShared(transformCollaborateRequest);
		} catch (MetadataException e) {
			Assert.assertEquals(403, e.getHttpStatus().getCode());
		}

	}
}
