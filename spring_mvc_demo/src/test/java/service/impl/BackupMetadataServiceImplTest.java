package service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import model.BackupMetadata;
import model.BackupMetadata4Modify;
import model.Metadata;
import model.MetadataAttr;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.context.ContextConfiguration;

import service.MetadataAttrService;
import utils.StringUtils;
import base.BaseTestCase;

import com.google.common.base.Function;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import constants.HttpStatus;
import constants.MetadataType;
import dao.BackupMetadataDao;
import exception.MetadataException;

@ContextConfiguration("backup_metadata.xml")
public class BackupMetadataServiceImplTest extends BaseTestCase {

	@Mock
	private BackupMetadataDao backupMetadataDao;

	@Mock
	private MetadataAttrService metadataAttrService;

	@InjectMocks
	private BackupMetadataServiceImpl impl;

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);

	}

	String backupRootId = "cd729388-68b8-4805-8aff-buckup000000";
	String user = "hardytest";

	@Test
	public void clearBackupNullTest() {
		List<BackupMetadata> unbackups = null;
		Mockito.when(backupMetadataDao.getAllUnbackupMetadatas(user))
				.thenReturn(unbackups);
		impl.deleteAllBackup(user);
	}

	@Test
	public void getParentIdsTest() {
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000005"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000003");
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000004"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000002");
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000003"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000002");
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000002"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000001");
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000001"))
				.thenReturn(backupRootId);
		Mockito.when(backupMetadataDao.getBackupParentId(backupRootId))
				.thenReturn(null);

		List<String> ids = impl
				.getParentIds("ad729388-68b8-4805-8aff-backup000005");
		Assert.assertArrayEquals(
				Arrays.asList("ad729388-68b8-4805-8aff-backup000003",
						"ad729388-68b8-4805-8aff-backup000002",
						"ad729388-68b8-4805-8aff-backup000001", backupRootId)
						.toArray(), ids.toArray());

	}

	@Test
	public void getParentIdsErrorDataTest() {
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000005"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000003");

		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000003"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000005");

		try {
			impl.getParentIds("ad729388-68b8-4805-8aff-backup000005");
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR,
					e.getHttpStatus());
		}

	}

	@Test
	public void clearBackupTest() {

		List<BackupMetadata> unbackups = new ArrayList<BackupMetadata>();

		unbackups.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000001", backupRootId, true,
				"ad729388-68b8-4805-8aff-origin000001"));
		unbackups.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000002",
				"ad729388-68b8-4805-8aff-backup000001", true,
				"ad729388-68b8-4805-8aff-origin000002"));
		unbackups.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000003",
				"ad729388-68b8-4805-8aff-backup000002", true,
				"ad729388-68b8-4805-8aff-origin000003"));
		unbackups.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000004",
				"ad729388-68b8-4805-8aff-backup000002", false,
				"ad729388-68b8-4805-8aff-origin000004"));
		unbackups.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000014",
				"ad729388-68b8-4805-8aff-backup000002", false,
				"ad729388-68b8-4805-8aff-origin000004"));

		unbackups.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000005",
				"ad729388-68b8-4805-8aff-backup000003", false,
				"ad729388-68b8-4805-8aff-origin000005"));

		Mockito.when(backupMetadataDao.getAllUnbackupMetadatas(user))
				.thenReturn(unbackups);

		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000005"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000003");
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000004"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000002");
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000003"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000002");
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000002"))
				.thenReturn("ad729388-68b8-4805-8aff-backup000001");
		Mockito.when(
				backupMetadataDao
						.getBackupParentId("ad729388-68b8-4805-8aff-backup000001"))
				.thenReturn(backupRootId);
		Mockito.when(backupMetadataDao.getBackupParentId(backupRootId))
				.thenReturn(null);

		Mockito.when(backupMetadataDao.getAllUnbackupMetadatas(user))
				.thenReturn(unbackups);

		List<MetadataAttr> attrs = new ArrayList<MetadataAttr>();
		Mockito.when(metadataAttrService.batchUpdateMetadataAttrOnOriginWithAspect(attrs))
				.thenReturn(attrs);
		impl.deleteAllBackup(user);
	}

	@Test
	public void updateBackupTest() {
		Metadata metadata = Mockito.mock(Metadata.class);
		Mockito.when(metadata.getId()).thenReturn("hardy123123");
		Mockito.when(metadata.getTotalSize()).thenReturn(200L);
		Mockito.when(metadata.getBlockId()).thenReturn(
				"11111111222222233333333");
		Mockito.when(metadata.getModifiedAt()).thenReturn(123456L);

		BackupMetadata backupMetadata = Mockito.mock(BackupMetadata.class);
		Mockito.when(backupMetadata.getTotalSize()).thenReturn(500L);
		Mockito.when(backupMetadata.getOriginalIndexId()).thenReturn(
				"hardy123123");

		MetadataAttr attr = Mockito.mock(MetadataAttr.class);
		Mockito.when(attr.getTotalSize()).thenReturn(200L - 500L);

		Mockito.when(backupMetadataDao.getLatestBackup("hardy123123"))
				.thenReturn(backupMetadata);

		Mockito.when(metadataAttrService.updateMetadataAttrOnOriginWithAspec(attr)).thenReturn(
				attr);
		Mockito.when(backupMetadataDao.updateBackup(backupMetadata))
				.thenReturn(backupMetadata);

		BackupMetadata result = impl.updateBackup(metadata);

		Assert.assertEquals(backupMetadata, result);
		Assert.assertEquals(result.getOriginalIndexId(), metadata.getId());

	}

	@Test
	public void registerRootBackupTest() {
		BackupMetadata root = Mockito.mock(BackupMetadata.class);
		Mockito.when(root.getId()).thenReturn(backupRootId);
		Mockito.when(impl.registerRootBackup(root)).thenReturn(root);
		BackupMetadata result = impl.registerRootBackup(root);
		Assert.assertEquals(root.getId(), result.getId());

	}

	@Test
	public void backupFolderTest() {
		List<Metadata> metadatas = new ArrayList<Metadata>();
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000001",
				"root", true));
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000002",
				"ad729388-68b8-4805-8aff-orgin0000001", true));
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000003",
				"ad729388-68b8-4805-8aff-orgin0000002", true));

		List<String> idList = Lists.newArrayList(Iterables.transform(metadatas,
				new Function<Metadata, String>() {
			@Override
			public String apply(Metadata input) {
				return input.getId();
			}
		}));
		
		List<BackupMetadata> oldBackup = new ArrayList<BackupMetadata>();
		oldBackup.add(createBackupMetadata(backupRootId, "", true, "root"));
		
		Mockito.when(backupMetadataDao.getBackups(idList))
				.thenReturn(oldBackup);

		mockBackupFile();

		List<BackupMetadata> backupdatas = impl.backupMetadata(metadatas, user);
		Assert.assertNotNull(backupdatas);
		//Assert.assertEquals(size, 0L);
	}

	@Test
	public void backupFileExistsTest() {
		List<Metadata> metadatas = new ArrayList<Metadata>();
		// folder path
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-origin000001",
				"root", true));
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-origin000002",
				"ad729388-68b8-4805-8aff-origin000001", true));
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-origin000003",
				"ad729388-68b8-4805-8aff-origin000002", true));
		//file origin000004
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-origin000004",
				"ad729388-68b8-4805-8aff-origin000002", false));
		//file origin000005
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-origin000005",
				"ad729388-68b8-4805-8aff-origin000003", false));
		
		//file origin000006
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-origin000006",
				"root", false));
		
		//file origin000007
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-origin000007",
				"ad729388-68b8-4805-8aff-origin000001", false));

		List<String> idList = Lists.newArrayList(Iterables.transform(metadatas,
				new Function<Metadata, String>() {
					@Override
					public String apply(Metadata input) {
						return input.getId();
					}
				}));

		List<BackupMetadata> oldBackup = new ArrayList<BackupMetadata>();

		oldBackup.add(createBackupMetadata(backupRootId, "", true, "root"));
		oldBackup.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000001", backupRootId, true,
				"ad729388-68b8-4805-8aff-origin000001"));
		oldBackup.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000002",
				"ad729388-68b8-4805-8aff-backup000001", true,
				"ad729388-68b8-4805-8aff-origin000002"));
		oldBackup.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000003",
				"ad729388-68b8-4805-8aff-backup000002", true,
				"ad729388-68b8-4805-8aff-origin000003"));
		
		//file origin000004 backed up
		oldBackup.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000004",
				"ad729388-68b8-4805-8aff-backup000002", false,
				"ad729388-68b8-4805-8aff-origin000004"));
		oldBackup.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000014",
				"ad729388-68b8-4805-8aff-backup000002", false,
				"ad729388-68b8-4805-8aff-origin000004"));

		oldBackup.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000024",
				"ad729388-68b8-4805-8aff-backup000002", false,
				"ad729388-68b8-4805-8aff-origin000004"));

		//file origin000005 backed up
		oldBackup.add(createBackupMetadata(
				"ad729388-68b8-4805-8aff-backup000005",
				"ad729388-68b8-4805-8aff-backup000003", false,
				"ad729388-68b8-4805-8aff-origin000005"));

		// oldBackup = Mockito.mock(oldBackup.getClass());
		//Mockito.when(backupMetadataDao.getRootId(user)).thenReturn(backupRootId);
		
		Mockito.when(backupMetadataDao.getBackups(idList))
				.thenReturn(oldBackup);

		List<BackupMetadata> olds = backupMetadataDao.getBackups(idList);
		Assert.assertEquals(oldBackup, olds);
		mockBackupFile();

		//long size = impl.backupMetadata(metadatas, user);
		//backupCount=3 file origin000004  have been backed up 3 file 
		//size -->  5,6,7 + 0
		//Assert.assertEquals(3000L, size);
		List<BackupMetadata> backupdatas = impl.backupMetadata(metadatas, user);
		Assert.assertNotNull(backupdatas);

	}


	@Test
	public void backupFileTest() {
		List<Metadata> metadatas = new ArrayList<Metadata>();
		// folder path
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000001",
				"root", true));
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000002",
				"ad729388-68b8-4805-8aff-orgin0000001", true));
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000003",
				"ad729388-68b8-4805-8aff-orgin0000002", true));
		// file
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000004",
				"ad729388-68b8-4805-8aff-orgin0000002", false));
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000005",
				"ad729388-68b8-4805-8aff-orgin0000003", false));
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000006",
				"root", false));
		metadatas.add(createMetadata("ad729388-68b8-4805-8aff-orgin0000007",
				"ad729388-68b8-4805-8aff-orgin0000001", false));

		List<String> idList = Lists.newArrayList(Iterables.transform(metadatas,
				new Function<Metadata, String>() {
					@Override
					public String apply(Metadata input) {
						return input.getId();
					}
				}));
		
		List<BackupMetadata> oldBackup = new ArrayList<BackupMetadata>();
		oldBackup.add(createBackupMetadata(backupRootId, "", true, "root"));
		
		Mockito.when(backupMetadataDao.getBackups(idList))
				.thenReturn(oldBackup);

		mockBackupFile();

		//long size = impl.backupMetadata(metadatas, user);
		//Assert.assertTrue(size >= 0 || size <= 4000);
		
		List<BackupMetadata> backupdatas = impl.backupMetadata(metadatas, user);
		Assert.assertNotNull(backupdatas);

	}

	@Test
	public void deleteBackupTest() {
		String backupMatedataId = "test id";
		impl.deleteBackup(backupMatedataId);
		try {
			impl.deleteBackup(null);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS,
					e.getHttpStatus());
		}
		try {
			impl.deleteBackup("");
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS,
					e.getHttpStatus());
		}
	}

	@Test
	public void deleteBackupNullParameterTest() {
		String backupMatedataIdString = null;
		try {
			impl.deleteBackup(backupMatedataIdString);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS,
					e.getHttpStatus());
		}

	}

	@Test
	public void unbackupFileTest() {
		String metadataOriginId = "test id";
		Mockito.when(backupMetadataDao.cancelBackupFile(metadataOriginId))
				.thenReturn(metadataOriginId);
		impl.unbackupMetadata(metadataOriginId);
		//Assert.assertEquals(metadataOriginId, result);

		try {
			impl.unbackupMetadata(null);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS,
					e.getHttpStatus());
		}

		try {
			impl.unbackupMetadata("");
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS,
					e.getHttpStatus());
		}

	}

	private void mockBackupFile() {

		List<BackupMetadata4Modify> modifies = new ArrayList<BackupMetadata4Modify>();
		List<BackupMetadata> creations = new ArrayList<BackupMetadata>();
		List<MetadataAttr> creationAttrs = new ArrayList<MetadataAttr>();
		List<MetadataAttr> modifyAttrs = new ArrayList<MetadataAttr>();

		Mockito.when(backupMetadataDao.bathUpdateBackupNo(modifies))
				.thenReturn(modifies);
		Mockito.when(backupMetadataDao.batchFileCreateBackup(creations))
				.thenReturn(creations);
		Mockito.when(
				metadataAttrService.batchUpdateMetadataAttrOnOrigin(modifyAttrs))
				.thenReturn(modifyAttrs);
		Mockito.when(metadataAttrService.batchCreateMetadataAttr(creationAttrs))
				.thenReturn(creationAttrs);
		Mockito.when(backupMetadataDao.getRootId(user))
				.thenReturn(backupRootId);
	}

	private Metadata createMetadata(String IndexId, String parentId,
			boolean isFolder) {
		Metadata metadata = new Metadata();
		metadata.setId(StringUtils.isNullOrEmpty(IndexId) ? StringUtils
				.getUUID() : IndexId);
		metadata.setBackupCount(3);
		metadata.setBlockId("abcdefg123123123123hijklmnopqrst123");
		metadata.setCreatedAt(System.currentTimeMillis());
		metadata.setCreatedBy("user123123123");
		metadata.setDeviceUid("1111111111111");
		metadata.setIconBlockId("qweqwewqeqweqweqwewe123123");
		metadata.setEncrypted(false);
		metadata.setFolder(isFolder);
		metadata.setFullSourcePath("FullSourcePath123123123123123123123");
		metadata.setModifiedAt(System.currentTimeMillis());
		metadata.setModifiedBy(user);
		metadata.setName(String.format("Test File-%s", metadata.getId())
				.toLowerCase());
		metadata.setOriginName(String.format("Test File-%s", metadata.getId()));
		metadata.setNote("notenotenotenotenotenotenotenote");
		metadata.setOwnerId(user);
		metadata.setParams("{a=1}");
		metadata.setParentId(parentId == null ? StringUtils.getUUID()
				: parentId);
		metadata.setSharedRootId(StringUtils.getUUID());
		metadata.setSize(isFolder ? 0L : 1000L);
		metadata.setSortPriority(2);
		metadata.setType(MetadataType.NORMAL.toString());
		return metadata;
	}

	private BackupMetadata createBackupMetadata(String IndexId,
			String parentId, boolean isFolder, String originIndexId) {
		BackupMetadata metadata = new BackupMetadata();
		metadata.setId(StringUtils.isNullOrEmpty(IndexId) ? StringUtils
				.getUUID() : IndexId);
		// metadata.setBackupCount(3);
		metadata.setBlockId("abcdefg123123123123hijklmnopqrst123");
		metadata.setCreatedAt(System.currentTimeMillis());
		metadata.setCreatedBy("user123123123");
		metadata.setDeviceUid("1111111111111");
		metadata.setIconBlockId("qweqwewqeqweqweqwewe123123");
		metadata.setEncrypted(false);
		metadata.setFolder(isFolder);
		metadata.setFullSourcePath("FullSourcePath123123123123123123123");
		metadata.setModifiedAt(System.currentTimeMillis());
		metadata.setModifiedBy(user);
		metadata.setName(String.format("Test File-%s", metadata.getId())
				.toLowerCase());
		metadata.setOriginName(String.format("Test File-%s", metadata.getId()));
		metadata.setNote("notenotenotenotenotenotenotenote");
		metadata.setOwnerId(user);
		metadata.setParams("{a=1}");
		metadata.setParentId(null == parentId ? StringUtils.getUUID(): parentId);
		metadata.setOriginalIndexId(originIndexId);
		metadata.setSize(isFolder ? 0L : 1000L);
		metadata.setSortPriority(2);
		metadata.setType(MetadataType.BACKUP.toString());
		return metadata;
	}

}
