package service.impl;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import model.Metadata;
import model.PathRequestBase;
import model.request.GetFolderRequest;
import model.request.UpdateFileRequest;
import model.response.GetFolderResponse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.kubek2k.springockito.annotations.ReplaceWithMock;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import service.BackupMetadataService;
import service.FileService;
import service.FolderService;
import base.BaseTestCase;
import constants.HttpStatus;
import dao.MetadataDao;
import exception.MetadataException;

@ContextConfiguration("folder.xml")
public class FolderServiceImplTest extends BaseTestCase {

	@Autowired
	private GetFolderRequest getFolderRequest;
	@Autowired
	private Metadata rootMetadataToAdd;
	@Autowired
	private Metadata firstLevelFolderMetadataToAdd;
	@Autowired
	private Metadata firstLevelFileMetadataToAdd;

	@Autowired
	private FileService fileService;
	@Autowired
	private UpdateFileRequest createLinkFileRequest;
	@Autowired
	private PathRequestBase deleteLinkFileRequest;
	@Autowired
	private PathRequestBase unlinkFolderRequest;
	@Autowired
	private PathRequestBase linkFolderRequest;
	// @Autowired
	// private PathRequestBase getSubBackupFolderCount;

	@Autowired
	@ReplaceWithMock
	private MetadataDao metadataDao;

	@Autowired
	@ReplaceWithMock
	private BackupMetadataService backupMetadataServiceImpl;

	@Autowired
	@InjectMocks
	private FolderService folderService;

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testGetFolder() {
		Mockito.when(
				metadataDao.getMetadataByPath(getFolderRequest.getUserId()
						.toLowerCase(), "", "")).thenReturn(rootMetadataToAdd);
		Metadata[] childMetadatas = new Metadata[6];
		Arrays.fill(childMetadatas, firstLevelFolderMetadataToAdd);
		childMetadatas[5] = firstLevelFileMetadataToAdd;
		Mockito.when(
				metadataDao.getMetadatasByParentId(rootMetadataToAdd.getId()))
				.thenReturn(Arrays.asList(childMetadatas));

		GetFolderResponse response = folderService.getFolder(getFolderRequest);
		Assert.assertEquals(2, response.getFolders().size());
		Assert.assertEquals(1, response.getFiles().size());
	}

	@Test
	public void testCreateFolder() {
		Mockito.when(
				metadataDao.getMetadataByPath(getFolderRequest.getUserId()
						.toLowerCase(), "", "")).thenReturn(rootMetadataToAdd);
		Metadata[] childMetadatas = new Metadata[6];
		Arrays.fill(childMetadatas, firstLevelFolderMetadataToAdd);
		childMetadatas[5] = firstLevelFileMetadataToAdd;
		Mockito.when(
				metadataDao.getMetadatasByParentId(rootMetadataToAdd.getId()))
				.thenReturn(Arrays.asList(childMetadatas));

		GetFolderResponse response = folderService.getFolder(getFolderRequest);
		Assert.assertEquals(2, response.getFolders().size());
		Assert.assertEquals(1, response.getFiles().size());
	}

	@Test
	public void testUnlinkFolder() {
		HttpStatus status = HttpStatus.OK;
		try{
			fileService.createFile(createLinkFileRequest);
			folderService.unlinkFolder(unlinkFolderRequest);
			fileService.deleteFile(deleteLinkFileRequest);
		}
		catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.OK);
	}

	@Test
	public void testLinkFolder() {
		HttpStatus status = HttpStatus.OK;
		try{
			fileService.createFile(createLinkFileRequest);
			folderService.linkFolder(linkFolderRequest);
			fileService.deleteFile(deleteLinkFileRequest);
		}
		catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.OK);
	}

	@Test
	public void testUnlinkFolders() {
		// TODO
	}

	@Test
	public void testLinkFolders() {
		// TODO
	}

	@Test
	public void testDeleteFolder() {

	}
	

//	 @Test
//	 public void testGetSubBackupCount() {
//	 List<GetSubBackupCountResponse> result = folderService
//	 .getSubBackupCount(getSubBackupFolderCount);
//	 }
}
