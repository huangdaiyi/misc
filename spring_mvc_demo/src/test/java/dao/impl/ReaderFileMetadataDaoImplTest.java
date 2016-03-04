package dao.impl;

import java.util.List;

import javax.annotation.Resource;

import model.Metadata;
import model.ReaderFileMetadata;
import model.ReaderPageDetail;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import base.BaseTestCase;
import dao.MetadataDao;
import dao.ReaderFileMetadataDao;

@ContextConfiguration({ "metadata.xml", "reader_file_metadata.xml" })
public class ReaderFileMetadataDaoImplTest extends BaseTestCase {

	@Autowired
	private Metadata rootMetadataToAdd;
	@Autowired
	private Metadata firstLevelMetadataToAdd;
	@Autowired
	private ReaderFileMetadata readerFileMetadataToAdd;
	@Resource(name = "readerPageDetailsToAdd")
	private List<ReaderPageDetail> readerPageDetailsToAdd;

	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private ReaderFileMetadataDao readerFileMetadataDao;

	@Before
	public void setup() {
		metadataDao.createMetadata(rootMetadataToAdd);
		metadataDao.createMetadata(firstLevelMetadataToAdd);
		readerFileMetadataDao.replaceReaderFileMetadata(readerFileMetadataToAdd);
		readerFileMetadataDao.replaceReaderPageDetail(readerFileMetadataToAdd.getMetadataIndexId(), readerPageDetailsToAdd);
	}

	@After
	public void after() {
		metadataDao.deleteMetadata(firstLevelMetadataToAdd.getId());
		metadataDao.deleteMetadata(rootMetadataToAdd.getId());
		readerFileMetadataDao.deleteReaderFileMetadataAndPageDetails(readerFileMetadataToAdd.getMetadataIndexId());
	}

	@Test
	public void testUpdateReaderFileMetadata() {
		readerFileMetadataToAdd.setConvertTo("pdf");
		readerPageDetailsToAdd.get(0).setSheetName("newSheetName");
		readerFileMetadataDao.replaceReaderFileMetadata(readerFileMetadataToAdd);
		readerFileMetadataDao.replaceReaderPageDetail(readerFileMetadataToAdd.getMetadataIndexId(), readerPageDetailsToAdd);

		ReaderFileMetadata readerFileMetadata = readerFileMetadataDao.getReaderFileMetadata(firstLevelMetadataToAdd.getId());
		Assert.assertEquals(readerFileMetadataToAdd.getConvertTo(), readerFileMetadata.getConvertTo());

		List<ReaderPageDetail> readerPageDetails = readerFileMetadataDao.getReaderPageDetails(firstLevelMetadataToAdd.getId());
		Assert.assertEquals(readerPageDetailsToAdd.size(), readerPageDetails.size());
		Assert.assertEquals(readerPageDetailsToAdd.get(0).getSheetName(), readerPageDetails.get(0).getSheetName());
	}

	@Test
	public void testGetReaderFileMetadata() {
		ReaderFileMetadata readerFileMetadata = readerFileMetadataDao.getReaderFileMetadata(firstLevelMetadataToAdd.getId());
		Assert.assertEquals(readerFileMetadataToAdd.getConvertTo(), readerFileMetadata.getConvertTo());

		List<ReaderPageDetail> readerPageDetails = readerFileMetadataDao.getReaderPageDetails(firstLevelMetadataToAdd.getId());
		Assert.assertEquals(readerPageDetailsToAdd.size(), readerPageDetails.size());
		Assert.assertEquals(readerPageDetailsToAdd.get(0).getSheetName(), readerPageDetails.get(0).getSheetName());
	}

}
