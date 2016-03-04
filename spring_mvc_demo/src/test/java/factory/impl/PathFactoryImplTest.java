package factory.impl;

import model.Metadata;
import model.PathInfo;
import model.PathRequestBase;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import service.CollaborateService;
import base.BaseTestCase;
import dao.MetadataDao;
import factory.PathFactory;

@ContextConfiguration("path_request.xml")
public class PathFactoryImplTest extends BaseTestCase {

	@Mock
	private CollaborateService collaborateService;
	
	@Mock
	private MetadataDao metadataDao;

	@Autowired
	@InjectMocks
	private PathFactory pathFactory;

	@Autowired
	private PathRequestBase normalFolderPathRequestBase;
	@Autowired
	private PathRequestBase syncFolderPathRequestBase;
	@Autowired
	private PathRequestBase syncFolderPathRequestBaseWithSourcePath;
	@Autowired
	private PathRequestBase sharedFolderPathRequestBase;
	@Autowired
	private Metadata normalMetadata;

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);
	}

//	@Test
//	public void testMappingToCommonFolderByExtension() {
//		Assert.assertEquals(CommonFolders.MY_PICTURES, pathFactory.mappingToCommonFolderByExtension(syncFolderPathRequestBase.getPath()));
//		Assert.assertEquals(CommonFolders.MY_PICTURES, pathFactory.mappingToCommonFolderByExtension(syncFolderPathRequestBaseWithSourcePath.getPath()));
//	}
//
	@Test
	public void testNormalFolderPath() {
		PathInfo pathInfo = new PathInfo(normalFolderPathRequestBase.getUserId(), normalFolderPathRequestBase.getUserId()+"/"+normalFolderPathRequestBase.getPath(), "", "", 
				normalFolderPathRequestBase.getOwnerId(), String.format("%s/%s", normalFolderPathRequestBase.getOwnerId(), normalFolderPathRequestBase.getPath()), "");
		pathInfo.setFullOriginPath(normalFolderPathRequestBase.getUserId()+"/"+normalFolderPathRequestBase.getPath());
		Mockito.when(metadataDao.getMetadataByPath(pathInfo.getFullOriginPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath())).thenReturn(normalMetadata);
		PathInfo pathInfo2 = pathFactory.parsePathInfo(normalFolderPathRequestBase, true, true);
		Assert.assertEquals(String.format("%s/%s", normalFolderPathRequestBase.getUserId(), normalFolderPathRequestBase.getPath()), pathInfo2.getFullOwnerPath());
	}
//
//	@Test
//	public void testSyncFolderPath() {
//		PathInfo pathInfo = pathFactory.parsePathInfo(syncFolderPathRequestBase);
//		Assert.assertEquals(syncFolderPathRequestBase.getDeviceId(), pathInfo.getDeviceUid());
//		Assert.assertEquals(String.format("%s/%s/%s", syncFolderPathRequestBase.getUserId(), CommonFolders.MY_PICTURES, FilenameUtils.getName(syncFolderPathRequestBase.getPath())), pathInfo.getFullOwnerPath());
//	}
//
//	@Test
//	public void testSyncFolderPathWithSourcePath() {
//		PathInfo pathInfo = pathFactory.parsePathInfo(syncFolderPathRequestBaseWithSourcePath);
//		Assert.assertEquals(syncFolderPathRequestBaseWithSourcePath.getDeviceId(), pathInfo.getDeviceUid());
//		Assert.assertEquals(String.format("%s/%s", syncFolderPathRequestBaseWithSourcePath.getUserId(), syncFolderPathRequestBaseWithSourcePath.getSourcePath()), pathInfo.getFullSourcePath());
//	}
//
	@Test
	public void testSharedFolderPath() {
		Mockito.when(collaborateService.canAccessCollaborate(sharedFolderPathRequestBase.getSharedRootId(), sharedFolderPathRequestBase.getOwnerId(),sharedFolderPathRequestBase.getUserId(),"")).thenReturn(true);

		PathInfo pathInfo = pathFactory.parsePathInfo(sharedFolderPathRequestBase, true, true);
		Assert.assertEquals(sharedFolderPathRequestBase.getOwnerId(), pathInfo.getOwnerId());
		Assert.assertTrue(pathInfo.isUnderSharedFolders());
	}

	@Test
	public void testSharedFolderPathForDenyUser() {
		Mockito.when(collaborateService.canAccessCollaborate(sharedFolderPathRequestBase.getSharedRootId(), sharedFolderPathRequestBase.getOwnerId(),sharedFolderPathRequestBase.getUserId(),"")).thenReturn(false);
		Mockito.when(metadataDao.getFullPath(sharedFolderPathRequestBase.getSharedRootId())).thenReturn(sharedFolderPathRequestBase.getOwnerId()+"/normalfolder");

		PathInfo pathInfo = pathFactory.parsePathInfo(sharedFolderPathRequestBase,true, true);
		Assert.assertEquals(sharedFolderPathRequestBase.getOwnerId(), pathInfo.getOwnerId());
		Assert.assertFalse(pathInfo.isUnderSharedFolders());
	}
//
//	@Test
//	public void testFetchDeviceUid() {
//		Assert.assertEquals("", pathFactory.fetchDeviceUid(null));
//		Assert.assertEquals(syncFolderPathRequestBase.getDeviceId(), pathFactory.fetchDeviceUid(syncFolderPathRequestBase.getPath()));
//		Assert.assertEquals(syncFolderPathRequestBaseWithSourcePath.getDeviceId(), pathFactory.fetchDeviceUid(syncFolderPathRequestBaseWithSourcePath.getSourcePath()));
//	}
//
//	@Test 
//	public void testCheckCanCreateFile() {
//		try {
//			pathFactory.checkCanCreateFile("My Backup Data/aaa.txt");
//			Assert.assertTrue(false);
//		} catch (MetadataException e) {
//			Assert.assertTrue(true);
//		}
//	}
//	
//	@Test 
//	public void testCheckCanCreateFolder() {
//		try {
//			pathFactory.checkCanCreateFolder("My Shared Folders/a_folder");
//			Assert.assertTrue(false);
//		} catch (MetadataException e) {
//			Assert.assertTrue(true);
//		}
//	}
//	
//	@Test 
//	public void testCheckCanCopyFolderFrom() {
//		try {
//			pathFactory.checkCanCopyFolderFrom("My Shared Folders");
//			Assert.assertTrue(false);
//		} catch (MetadataException e) {
//			Assert.assertTrue(true);
//		}
//	}
}
