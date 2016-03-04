package dao.impl;

import model.Metadata;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import base.BaseTestCase;
import dao.FolderDao;
import dao.MetadataDao;

@ContextConfiguration("metadata.xml")
public class FolderDaoImplTest extends BaseTestCase {

	@Autowired
	private Metadata rootMetadataToAdd;
	@Autowired
	private Metadata firstLevelMetadataToAdd;

	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private FolderDao folderDao;

	@Before
	public void setup() {
		metadataDao.createMetadata(rootMetadataToAdd);
		metadataDao.createMetadata(firstLevelMetadataToAdd);
	}

	@After
	public void after() {
		metadataDao.deleteMetadata(firstLevelMetadataToAdd.getId());
		metadataDao.deleteMetadata(rootMetadataToAdd.getId());
	}

	@Test
	public void testGetFolderIdByCriteria() {
		String id = folderDao.getFolderIdByCriteria(rootMetadataToAdd.getId(), firstLevelMetadataToAdd.getName(), firstLevelMetadataToAdd.getDeviceUid(), firstLevelMetadataToAdd.getFullSourcePath());
		Assert.assertEquals(firstLevelMetadataToAdd.getId(), id);
	}

}
