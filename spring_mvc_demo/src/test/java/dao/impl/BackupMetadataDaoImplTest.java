package dao.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import model.BackupMetadata;
import model.MetadataAttr;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import utils.DateUtils;
import utils.StringUtils;
import base.BaseTestCase;

import com.google.common.base.Function;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import constants.MetadataType;
import dao.BackupMetadataDao;
import dao.MetadataAttrDao;

public class BackupMetadataDaoImplTest extends BaseTestCase {

	@Autowired
	private BackupMetadataDao backupMetadataDao;

	@Autowired
	private MetadataAttrDao metadataAttrDao;

	@Test
	public void createFileBuckupTest() {
		BackupMetadata backupMetadata = createBackupMetadata(
				"cd729388-68b8-4805-back-000000000002",
				"cd729388-68b8-4805-back-000000000001", false);
		BackupMetadata result = backupMetadataDao
				.createFileBuckup(backupMetadata);

		// delete data
		backupMetadataDao.deleteBackupById(backupMetadata.getId());

		Assert.assertEquals(backupMetadata, result);
	}

	@Test
	public void createFolderBuckupTest() {
		BackupMetadata backupMetadata = createBackupMetadata(
				"cd729388-68b8-4805-back-000000000002",
				"cd729388-68b8-4805-back-000000000001", true);
		BackupMetadata result = backupMetadataDao
				.createFolderBackup(backupMetadata);

		// delete data
		backupMetadataDao.deleteBackupById(backupMetadata.getId());

		Assert.assertEquals(backupMetadata, result);

	}

	@Test
	public void batchFileCreateBackupTest() {
		List<BackupMetadata> backupMetadatas = new ArrayList<BackupMetadata>();
		backupMetadatas.add(createBackupMetadata(
				"cd729388-68b8-4805-back-000000000001", "root", true));
		backupMetadatas.add(createBackupMetadata(
				"cd729388-68b8-4805-back-000000000002",
				"cd729388-68b8-4805-back-000000000001", true));
		backupMetadatas.add(createBackupMetadata(
				"cd729388-68b8-4805-back-000000000003",
				"cd729388-68b8-4805-back-000000000002", true));
		backupMetadatas.add(createBackupMetadata(
				"cd729388-68b8-4805-back-000000000004",
				"cd729388-68b8-4805-back-000000000003", true));
		backupMetadatas.add(createBackupMetadata(
				"cd729388-68b8-4805-back-000000000005",
				"cd729388-68b8-4805-back-000000000004", false));
		backupMetadatas.add(createBackupMetadata(
				"cd729388-68b8-4805-back-000000000006",
				"cd729388-68b8-4805-back-000000000003", false));
		List<BackupMetadata> results = backupMetadataDao
				.batchFileCreateBackup(backupMetadatas);

		// clear data
		for (BackupMetadata backupMetadata : results) {
			backupMetadataDao.deleteBackupById(backupMetadata.getId());
		}

		Assert.assertEquals(backupMetadatas, results);

	}

	@Test
	public void getRootIdTest() {

		String backupRootId = "cd729388-68b8-4805-back-00000000root";
		String user = "hardy test";
		BackupMetadata root = createBackupMetadata(backupRootId, "", true);

		root.setOwnerId(user);
		backupMetadataDao.createFolderBackup(root);
		String rootId = backupMetadataDao.getRootId(user);

		backupMetadataDao.deleteBackupById(backupRootId);
		Assert.assertEquals(backupRootId, rootId);
	}

	@Test
	public void getBackupParentIdTest() {

		BackupMetadata backupMetadata = createBackupMetadata(
				"cd729388-68b8-4805-back-000000000002",
				"cd729388-68b8-4805-back-000000000001", false);

		backupMetadataDao.createFileBuckup(backupMetadata);

		String parentId = backupMetadataDao.getBackupParentId(backupMetadata
				.getId());

		backupMetadataDao.deleteBackupById(backupMetadata.getId());
		Assert.assertEquals(backupMetadata.getParentId(), parentId);

	}

	@Test
	public void getBackupIndexIdTest() {
		BackupMetadata backupMetadata = createBackupMetadata(
				"cd729388-68b8-4805-back-000000000002",
				"cd729388-68b8-4805-back-000000000001", false);
		String user = "hardy test";
		backupMetadata.setOwnerId(user);
		backupMetadataDao.createFileBuckup(backupMetadata);

		String id = backupMetadataDao.getBackupIndexId(user,
				backupMetadata.getName(), backupMetadata.getParentId());

		backupMetadataDao.deleteBackupById(backupMetadata.getId());

		Assert.assertEquals(backupMetadata.getId(), id);
	}

	@Test
	public void getLatestBackupTest() {

		String originIndexId = "cd729388-68b8-4805-back-000000origin";

		BackupMetadata backupMetadata1 = createBackupMetadata(
				"cd729388-68b8-4805-back-000000000002",
				"cd729388-68b8-4805-back-000000000001", false);
		backupMetadata1.setBackupNo(2);
		backupMetadata1.setOriginalIndexId(originIndexId);
		backupMetadataDao.createFileBuckup(backupMetadata1);

		BackupMetadata backupMetadata2 = createBackupMetadata(
				"cd729388-68b8-4805-back-000000000003",
				"cd729388-68b8-4805-back-000000000001", false);

		backupMetadata2.setBackupNo(1);
		backupMetadata2.setOriginalIndexId(originIndexId);
		backupMetadataDao.createFileBuckup(backupMetadata2);

		List<MetadataAttr> metadataAttrs = new ArrayList<MetadataAttr>();
		metadataAttrs.add(new MetadataAttr(
				"cd729388-68b8-4805-back-000000000001", true, 0, 2, 2000L));
		metadataAttrs.add(new MetadataAttr(
				"cd729388-68b8-4805-back-000000000002", true, 0, 0, 1000L));
		metadataAttrs.add(new MetadataAttr(
				"cd729388-68b8-4805-back-000000000003", true, 0, 0, 1000L));
		metadataAttrDao.batchCreateMetadataAttr(metadataAttrs);

		BackupMetadata result = backupMetadataDao
				.getLatestBackup(originIndexId);

		backupMetadataDao.deleteBackupById(backupMetadata1.getId());
		backupMetadataDao.deleteBackupById(backupMetadata2.getId());

		metadataAttrDao.deleteMetadataAttrs(Arrays.asList(
				backupMetadata1.getId(), backupMetadata2.getId(),
				backupMetadata1.getParentId()));

		Assert.assertEquals(backupMetadata2.getId(), result.getId());

	}

	@Test
	public void updateBackupTest() {

		BackupMetadata backup = createBackupMetadata(StringUtils.getUUID(),
				StringUtils.getUUID(), false);
		backupMetadataDao.createFileBuckup(backup);
		backup.setBackupNo(1);
		backup.setBlockId("modify bloackid");
		backup.setName("modify name");
		backup.setOriginName("modify original name");
		backup.setModifiedAt(DateUtils.nowUTCTimestamp());
		backupMetadataDao.updateBackup(backup);
		metadataAttrDao.createMetadataAttr(new MetadataAttr(backup.getId(),
				true));

		BackupMetadata modifiedBackup = backupMetadataDao.getBackup(backup
				.getId());

		backupMetadataDao.deleteBackupById(backup.getId());
		metadataAttrDao.deleteMetadataAttr(backup.getId());

		Assert.assertEquals(backup.getBackupNo(), modifiedBackup.getBackupNo());
		Assert.assertEquals(backup.getName(), modifiedBackup.getName());
		Assert.assertEquals(backup.getOriginName(),
				modifiedBackup.getOriginName());
		Assert.assertEquals(backup.getModifiedAt(),
				modifiedBackup.getModifiedAt());
	}

	@Test
	public void getUnbackupMetadatasTest() {
		List<BackupMetadata> backupMetadatas = new ArrayList<BackupMetadata>();
		String user = "hardy test";
		backupMetadatas.add(createBackupMetadata(
				"cd729388-68b8-4805-back-000000000001", "", true));
		BackupMetadata tempNackupMetadata = null;
		tempNackupMetadata = createBackupMetadata(
				"cd729388-68b8-4805-back-000000000002",
				"cd729388-68b8-4805-back-000000000001", false);
		tempNackupMetadata.setOriginalIndexId("0");
		tempNackupMetadata.setOwnerId(user);

		backupMetadatas.add(tempNackupMetadata);
		tempNackupMetadata = createBackupMetadata(
				"cd729388-68b8-4805-back-000000000003",
				"cd729388-68b8-4805-back-000000000001", false);

		backupMetadataDao.batchFileCreateBackup(backupMetadatas);
		tempNackupMetadata.setOriginalIndexId("0");
		tempNackupMetadata.setOwnerId(user);
		backupMetadatas.add(tempNackupMetadata);

		List<MetadataAttr> metadataAttrs = new ArrayList<MetadataAttr>();
		metadataAttrs.add(new MetadataAttr(
				"cd729388-68b8-4805-back-000000000001", true, 0, 2, 2000L));
		metadataAttrs.add(new MetadataAttr(
				"cd729388-68b8-4805-back-000000000002", true, 0, 0, 1000L));
		metadataAttrs.add(new MetadataAttr(
				"cd729388-68b8-4805-back-000000000003", true, 0, 0, 1000L));
		metadataAttrDao.batchCreateMetadataAttr(metadataAttrs);

		List<BackupMetadata> results = backupMetadataDao
				.getAllUnbackupMetadatas(user);

		for (BackupMetadata backupMetadata : backupMetadatas) {
			backupMetadataDao.deleteBackupById(backupMetadata.getId());
			metadataAttrDao.deleteMetadataAttr(backupMetadata.getId());
		}

		List<String> unbackupMetadataIds = Arrays.asList(
				"cd729388-68b8-4805-back-000000000002",
				"cd729388-68b8-4805-back-000000000003");

		for (BackupMetadata result : results) {
			Assert.assertTrue(unbackupMetadataIds.contains(result.getBlockId()));
		}

	}

	@Test
	public void testExistBackupName() {
		List<BackupMetadata> backupMetadatas = new ArrayList<BackupMetadata>();
		backupMetadatas.add(createBackupMetadata(
				"cd729388-68b8-4805-back-000000000001", "root", true));
		backupMetadatas.add(createBackupMetadata(
				"cd729388-68b8-4805-back-000000000002",
				"cd729388-68b8-4805-back-000000000001", true));

		String name = "hardy_20150325_1.jpg";

		BackupMetadata backupMetadata = createBackupMetadata(
				"cd729388-68b8-4805-back-000000000003",
				"cd729388-68b8-4805-back-000000000002", false);

		backupMetadata.setOriginalIndexId("");
		backupMetadata.setBackupNo(1);
		backupMetadata.setName(name);
		backupMetadata.setOriginName(name);
		backupMetadatas.add(backupMetadata);
		backupMetadataDao.batchFileCreateBackup(backupMetadatas);
		List<BackupMetadata> result = backupMetadataDao.ExistBackupName(name,
				backupMetadata.getParentId(), backupMetadata.getDeviceUid());
		Assert.assertEquals(0, result.size());

		List<BackupMetadata> result2 = backupMetadataDao.ExistBackupName(
				"hardy_20150325.jpg", backupMetadata.getParentId(),backupMetadata.getDeviceUid());
		Assert.assertEquals(1, result2.size());
		Assert.assertEquals(name, result2.get(0).getName());
		// clear data
		backupMetadataDao.deleteBackupByIds(Lists.newArrayList(Iterables
				.transform(backupMetadatas,
						new Function<BackupMetadata, String>() {

							@Override
							public String apply(BackupMetadata item) {
								return item.getId();
							}

						})));

	}

	private BackupMetadata createBackupMetadata(String IndexId,
			String parentId, boolean isFolder) {
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
		metadata.setModifiedBy("user123123123");
		metadata.setName(String.format("Test File-%s", metadata.getId())
				.toLowerCase());
		metadata.setOriginName(String.format("Test File-%s", metadata.getId()));
		metadata.setNote("notenotenotenotenotenotenotenote");
		metadata.setOwnerId("user123123123");
		metadata.setParams("{a=1}");
		metadata.setParentId(parentId == null ? StringUtils.getUUID()
				: parentId);
		metadata.setOriginalIndexId(StringUtils.getUUID());
		metadata.setTotalSize(isFolder ? 0L : 1000L);
		metadata.setSortPriority(2);
		metadata.setType(MetadataType.NORMAL.toString());
		return metadata;
	}
}
