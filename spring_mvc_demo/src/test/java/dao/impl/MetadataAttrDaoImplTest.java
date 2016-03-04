package dao.impl;

import java.util.ArrayList;
import java.util.List;

import model.BackupMetadata;
import model.MetadataAttr;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import utils.StringUtils;
import base.BaseTestCase;

import com.google.common.base.Function;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import constants.MetadataType;
import dao.BackupMetadataDao;
import dao.MetadataAttrDao;

public class MetadataAttrDaoImplTest extends BaseTestCase {
	
	@Autowired
	private MetadataAttrDao metadataAttrDao;
	
	@Autowired
	private BackupMetadataDao backupMetadataDao;
	
	private List<BackupMetadata> baucBackupMetadatas;
	
	List<MetadataAttr> metadataAdditionalAttrs;
	
	@Before
	public void initData(){
		
		baucBackupMetadatas = new ArrayList<BackupMetadata>();
		baucBackupMetadatas.add(createMetadata("cd729388-68b8-4805-attr-000000000001","root",true));
		baucBackupMetadatas.add(createMetadata("cd729388-68b8-4805-attr-000000000002","cd729388-68b8-4805-attr-000000000001",true));
		baucBackupMetadatas.add(createMetadata("cd729388-68b8-4805-attr-000000000003","cd729388-68b8-4805-attr-000000000002",true));
		baucBackupMetadatas.add(createMetadata("cd729388-68b8-4805-attr-000000000004","cd729388-68b8-4805-attr-000000000003",true));
		baucBackupMetadatas.add(createMetadata("cd729388-68b8-4805-attr-000000000005","cd729388-68b8-4805-attr-000000000004",false));
		baucBackupMetadatas.add(createMetadata("cd729388-68b8-4805-attr-000000000006","cd729388-68b8-4805-attr-000000000003",false));
		backupMetadataDao.batchFileCreateBackup(baucBackupMetadatas);
		metadataAdditionalAttrs = new ArrayList<MetadataAttr>();
		metadataAdditionalAttrs.add(new MetadataAttr("cd729388-68b8-4805-attr-000000000001", true, 0, 0, 2000L));
		metadataAdditionalAttrs.add(new MetadataAttr("cd729388-68b8-4805-attr-000000000002", true, 0, 0, 2000L));
		metadataAdditionalAttrs.add(new MetadataAttr("cd729388-68b8-4805-attr-000000000003", true, 0, 0, 2000L));
		metadataAdditionalAttrs.add(new MetadataAttr("cd729388-68b8-4805-attr-000000000004", true, 0, 0, 1000L));
		metadataAdditionalAttrs.add(new MetadataAttr("cd729388-68b8-4805-attr-000000000005", true, 0, 0, 1000L));
		metadataAdditionalAttrs.add(new MetadataAttr("cd729388-68b8-4805-attr-000000000006", true, 0, 0, 1000L));
		metadataAttrDao.batchCreateMetadataAttr(metadataAdditionalAttrs);
	}
	
	@After
	public void clearTestData(){
		
		List<String> idList = Lists.newArrayList( Iterables.transform(baucBackupMetadatas, new Function<BackupMetadata, String>(){
			@Override
			public String apply(BackupMetadata item) {
				return item.getId();
			}
			
		}));
		
		for (String id : idList) {
			backupMetadataDao.deleteBackupById(id);
		}
		metadataAttrDao.deleteMetadataAttrs(idList);
	}
	
	
	@Test
	public void updateMetadataAttrTest(){
		MetadataAttr attr = new MetadataAttr("cd729388-68b8-4805-attr-000000000006", true, 0, 0, -100L);
		MetadataAttr resultAttr = metadataAttrDao.updateMetadataAttrOnOriginWithAspec(attr);
		Assert.assertEquals(attr, resultAttr);
		//metadataAttrDao.
	}
	
	
	@Test 
	public void updateMetadataAttrsTest(){
		List<MetadataAttr> attrs = new ArrayList<MetadataAttr>();
		attrs.add(new MetadataAttr("cd729388-68b8-4805-attr-000000000005", true, 0, 0, -500L));
		attrs.add(new MetadataAttr("cd729388-68b8-4805-attr-000000000006", true, 0, 0, -400L));
		List<MetadataAttr> resultAttrs = metadataAttrDao.batchUpdateMetadataAttrOnOriginWithAspect(attrs);
		Assert.assertEquals(attrs, resultAttrs);
	}
	
	
	
	private BackupMetadata createMetadata(String IndexId, String parentId,
			boolean isFolder) {
		BackupMetadata metadata = new BackupMetadata();
		metadata.setId(StringUtils.isNullOrEmpty(IndexId) ? StringUtils
				.getUUID() : IndexId);
		//metadata.setBackupCount(3);
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
		metadata.setParentId(StringUtils.isNullOrEmpty(parentId) ? StringUtils
				.getUUID() : parentId);
		metadata.setOriginalIndexId(StringUtils.getUUID());
		metadata.setTotalSize(isFolder ? 0L : 1000L);
		metadata.setSortPriority(2);
		metadata.setType(MetadataType.NORMAL.toString());
		return metadata;
	}

	
}
