package dao.impl;

import model.Metadata;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import base.BaseTestCase;
import constants.MetadataType;
import dao.MetadataDao;

@ContextConfiguration("metadata.xml")
public class MetadataDaoImplTest extends BaseTestCase {

	@Autowired
	private Metadata rootMetadataToAdd;
	@Autowired
	private Metadata firstLevelMetadataToAdd;
	@Autowired
	private Metadata secondLevelMetadataToAdd;

	@Autowired
	private MetadataDao metadataDao;

	@Before
	public void setup() {
		metadataDao.createMetadata(rootMetadataToAdd);
		metadataDao.createMetadata(firstLevelMetadataToAdd);
		metadataDao.createMetadata(secondLevelMetadataToAdd);
	}

	@After
	public void after() {
		metadataDao.deleteMetadata(secondLevelMetadataToAdd.getId());
		metadataDao.deleteMetadata(firstLevelMetadataToAdd.getId());
		metadataDao.deleteMetadata(rootMetadataToAdd.getId());
	}

	@Test
	public void testCreateMetadata() {
		Metadata firstLevelMetadata = metadataDao.getMetadata(firstLevelMetadataToAdd.getId());
		Metadata secondLevelMetadata = metadataDao.getMetadata(secondLevelMetadataToAdd.getId());
		Assert.assertNotNull(firstLevelMetadata);
		Assert.assertNotNull(secondLevelMetadata);
	}

	@Test
	public void testGetMetadata() {
		Metadata firstLevelMetadata = metadataDao.getMetadata(firstLevelMetadataToAdd.getId());
		Metadata secondLevelMetadata = metadataDao.getMetadata(secondLevelMetadataToAdd.getId());
		Assert.assertNotNull(firstLevelMetadata);
		Assert.assertNotNull(secondLevelMetadata);
	}

	@Test
	public void testGetMetadataByPath() {
		Metadata firstLevelMetadata = metadataDao.getMetadataByPath("testOwner/testFirstLevelName", "", "");
		Metadata secondLevelMetadata = metadataDao.getMetadataByPath("testOwner/testFirstLevelName/testSecondLevelName", "", "");
		Assert.assertNotNull(firstLevelMetadata);
		Assert.assertNotNull(secondLevelMetadata);
		Assert.assertEquals("testFirstLevelName", firstLevelMetadata.getName());
		Assert.assertEquals("testSecondLevelName", secondLevelMetadata.getName());
		Assert.assertTrue(firstLevelMetadata.isFolder());
		Assert.assertFalse(secondLevelMetadata.isFolder());
		Assert.assertEquals(MetadataType.NORMAL.toString(), secondLevelMetadata.getType());
	}

}
