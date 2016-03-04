package factory.impl;

import java.util.ArrayList;
import java.util.List;

import model.Metadata;
import model.PageProfileProperty;
import model.response.FileResponse;
import model.response.FolderResponse;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import base.BaseTestCase;
import constants.SortType;
import constants.ViewType;
import dao.MetadataDao;
import factory.MetadataFactory;

@ContextConfiguration("metadata.xml")
public class MetadataFactoryImplTest extends BaseTestCase {

	@Autowired
	private Metadata fileMetadataToAdd;
	@Autowired
	private Metadata folderMetadataToAdd;

	@Autowired
	private MetadataFactory metadataFactory;
	@Autowired
	private MetadataDao metadataDao;

	@Before
	public void setup() {
		metadataDao.createMetadata(fileMetadataToAdd);
		metadataDao.createMetadata(folderMetadataToAdd);
	}

	@After
	public void after() {
		metadataDao.deleteMetadata(fileMetadataToAdd.getId());
		metadataDao.deleteMetadata(folderMetadataToAdd.getId());
	}

	@Test
	public void testToFolderResponse() {
		FolderResponse folderResponse = metadataFactory.toFolderResponse(folderMetadataToAdd, false, "");
		Assert.assertEquals(folderMetadataToAdd.getOriginName(), folderResponse.getName());
	}

	@Test
	public void testToFileResponse() {
		FileResponse fileResponse = metadataFactory.toFileResponse(fileMetadataToAdd, false, "");
		Assert.assertEquals(fileMetadataToAdd.getOriginName(), fileResponse.getName());
	}

	@Test
	public void testToFolderResponses() {
		List<Metadata> folders = new ArrayList<Metadata>();
		folders.add(folderMetadataToAdd);
		folders.add(folderMetadataToAdd);

		List<FolderResponse> folderResponses = metadataFactory.toFolderResponses(folders, false, "");
		Assert.assertEquals(2, folderResponses.size());
	}

	@Test
	public void testToFileResponses() {
		List<Metadata> files = new ArrayList<Metadata>();
		files.add(fileMetadataToAdd);
		files.add(fileMetadataToAdd);

		List<FileResponse> fileResponses = metadataFactory.toFileResponses("", files, false, "");
		Assert.assertEquals(2, fileResponses.size());
	}

	@Test
	public void testSortFolderResponses() {
		List<Metadata> folders = new ArrayList<Metadata>();
		folders.add(folderMetadataToAdd);
		folders.add(folderMetadataToAdd);

		List<FolderResponse> folderResponses = metadataFactory.toFolderResponses(folders, false, "");
		metadataFactory.sortFolderResponses(folderResponses, SortType.NAME);
		Assert.assertEquals(2, folderResponses.size());
	}

	@Test
	public void testSortFileResponses() {
		List<Metadata> files = new ArrayList<Metadata>();
		files.add(fileMetadataToAdd);
		files.add(fileMetadataToAdd);

		List<FileResponse> fileResponses = metadataFactory.toFileResponses("", files, false, "");
		metadataFactory.sortFileResponses(fileResponses, SortType.USER_ARRANGE);
		Assert.assertEquals(2, fileResponses.size());
	}

	@Test
	public void testPagingFolderResponses() {
		List<Metadata> folders = new ArrayList<Metadata>();
		for (int i = 0; i < 5; i++) {
			folders.add(folderMetadataToAdd);
		}
		List<FolderResponse> folderResponses = metadataFactory.toFolderResponses(folders, false, "");
		folderResponses = metadataFactory.pagingFolderResponses(folderResponses, 2, 3, 0);
		Assert.assertEquals(2, folderResponses.size());
	}

	@Test
	public void testPagingFileResponses() {
		List<Metadata> files = new ArrayList<Metadata>();
		for (int i = 0; i < 5; i++) {
			files.add(fileMetadataToAdd);
		}
		List<FileResponse> fileResponses = metadataFactory.toFileResponses("", files, false, "");
		fileResponses = metadataFactory.pagingFileResponses(fileResponses, 2, 3, 5);
		Assert.assertEquals(1, fileResponses.size());
	}

	@Test
	public void testComputeSortByType() {
		PageProfileProperty pageProfileProperty = metadataFactory.computePageProfileProperty(null, null, null, null);
		Assert.assertEquals(SortType.CREATED_AT, pageProfileProperty.getSortByType());
		Assert.assertEquals(ViewType.UNDEFINED, pageProfileProperty.getViewByType());
	}

}
