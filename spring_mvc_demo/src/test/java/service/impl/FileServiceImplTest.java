package service.impl;

import model.BackupMetadata;
import model.Metadata;
import model.PathInfo;
import model.PathRequestBase;
import model.ReaderFileMetadata;
import model.request.GetFileRequest;
import model.request.UpdateBackupCountRequest;
import model.request.UpdateFileRequest;
import model.request.UpdateNoteRequest;
import model.request.UpdateReaderFileMetadataRequest;
import model.response.GetReaderFileMetadataResponse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.kubek2k.springockito.annotations.ReplaceWithMock;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import service.FileService;
import base.BaseTestCase;
import constants.HttpStatus;
import dao.BackupMetadataDao;
import dao.MetadataDao;
import dao.ReaderFileMetadataDao;
import exception.MetadataException;
import factory.PathFactory;

@ContextConfiguration("file.xml")
public class FileServiceImplTest extends BaseTestCase {

	@InjectMocks
	@Autowired
	private FileService fileService;

	@Autowired
	@ReplaceWithMock
	private MetadataDao metadataDao;
	@Autowired
	@ReplaceWithMock
	private ReaderFileMetadataDao readerFileMetadataDao;
	@Autowired
	@ReplaceWithMock
	private BackupMetadataDao backupMetadataDao;

	@Autowired
	private UpdateBackupCountRequest updateBackupCountRequest;
	@Autowired
	private PathFactory pathFactory;
	@Autowired
	private UpdateFileRequest updateFileRequest;
	@Autowired
	private GetFileRequest getFileRequest;
	@Autowired
	private PathRequestBase deleteFileRequest;
	@Autowired
	private UpdateNoteRequest updateNoteRequest;
	@Autowired
	private PathRequestBase getNoteRequest;
	@Autowired
	private PathRequestBase getReaderFileMetadataRequest;
	@Autowired
	private UpdateReaderFileMetadataRequest updateReaderFileMetadataRequest;
/*	@Autowired
	private MoveFileRequest moveFileRequest;
	@Autowired
	private CopyFileRequest copyFileRequest;*/

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testFileCreate() {
		fileService.createFile(updateFileRequest);
		Assert.assertNotNull(fileService.getFile(getFileRequest));
		fileService.deleteFile(deleteFileRequest);
	}
	
	@Test
	public void testFileDelete() {
		fileService.createFile(updateFileRequest);
		fileService.deleteFile(deleteFileRequest);
		try {
			fileService.getFile(getFileRequest);
			Assert.assertTrue(false);
		} catch(MetadataException e) {
			Assert.assertEquals(HttpStatus.FILE_NOT_EXIST, e.getHttpStatus());
		} 
	}
	
	@Test
	public void testFileUpdateNoteAndGetNote() {
		fileService.createFile(updateFileRequest);
		fileService.updateNote(updateNoteRequest);
		Assert.assertEquals(updateNoteRequest.getNote(), fileService.getNote(getNoteRequest).getNote());
		fileService.deleteFile(deleteFileRequest);
	}

	@Test
	public void testGetReaderFileMetadataForBadData() {
		PathInfo pathInfo = pathFactory.parsePathInfo(getReaderFileMetadataRequest, false, true);
		Mockito.when(metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath())).thenReturn(null);
		try {
			fileService.getReaderFileMetadata(getReaderFileMetadataRequest);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.FILE_NOT_EXIST, e.getHttpStatus());
		}
	}

	@Test
	public void testGetReaderFileMetadata() {
		PathInfo pathInfo = pathFactory.parsePathInfo(getReaderFileMetadataRequest, false, true);
		Metadata metadata = new Metadata();
		Mockito.when(metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath())).thenReturn(metadata);
		ReaderFileMetadata readerFileMetadata = new ReaderFileMetadata();
		Mockito.when(readerFileMetadataDao.getReaderFileMetadata("")).thenReturn(readerFileMetadata);

		GetReaderFileMetadataResponse response = fileService.getReaderFileMetadata(getReaderFileMetadataRequest);
		Assert.assertEquals(readerFileMetadata.getConvertTo(), response.getConvertTo());
	}

	@Test
	public void testUpdateReaderFileMetadataForBadData() {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateReaderFileMetadataRequest,false, true);
		Mockito.when(metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath())).thenReturn(null);
		try {
			fileService.updateReaderFileMetadata(updateReaderFileMetadataRequest);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.FILE_NOT_EXIST, e.getHttpStatus());
		}
	}

	@Test
	public void testUpdateReaderFileMetadata() {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateReaderFileMetadataRequest, false, true);
		Metadata metadata = new Metadata();
		Mockito.when(metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath())).thenReturn(metadata);
		Mockito.when(readerFileMetadataDao.replaceReaderFileMetadata(null)).thenReturn(null);
		Mockito.when(readerFileMetadataDao.replaceReaderPageDetail("", null)).thenReturn(null);
		BackupMetadata backupMetadata = new BackupMetadata();
		Mockito.when(backupMetadataDao.getLatestBackup("")).thenReturn(backupMetadata);

		fileService.updateReaderFileMetadata(updateReaderFileMetadataRequest);
	}

/*	@Test
	public void testFileMove() {
		fileService.createFile(updateFileRequest);
		Assert.assertNotNull(fileService.getFile(getFileRequest));
		fileService.moveFile(moveFileRequest);
		Assert.assertNull(fileService.getFile(getFileRequest));
	}
	
	@Test
	public void testFileCopy() {
		fileService.createFile(updateFileRequest);
		Assert.assertNotNull(fileService.getFile(getFileRequest));
		fileService.copyFile(copyFileRequest);
		Assert.assertNotNull(fileService.getFile(getFileRequest));
	}*/
	
	@Test
	public void testFileUpdateBackupCount() {
		PathInfo pathInfo = pathFactory.parsePathInfo(updateBackupCountRequest,false, true);
		Metadata metadata = new Metadata();
		metadata.setId("c03aef6a-8786-432f-9932-604091833521");
		Mockito.when(metadataDao.getMetadataByPath(
				pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(),
				pathInfo.getFullSourcePath())).thenReturn(metadata);
		fileService.updateBackupCount(updateBackupCountRequest);
	}
}
