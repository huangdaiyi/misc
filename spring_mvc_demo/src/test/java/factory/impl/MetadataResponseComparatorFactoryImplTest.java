package factory.impl;

import java.util.Comparator;

import model.response.FileResponse;
import model.response.FolderResponse;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import base.BaseTestCase;
import constants.SortType;
import factory.MetadataResponseComparatorFactory;

public class MetadataResponseComparatorFactoryImplTest extends BaseTestCase {

	@Autowired
	private MetadataResponseComparatorFactory metadataResponseComparatorFactory;

	@Test
	public void testGenerateFolderResponseComparator() {
		Comparator<FolderResponse> folderResponseComparator = metadataResponseComparatorFactory.generateFolderResponseComparator(SortType.NAME);
		Assert.assertNotNull(folderResponseComparator);
	}

	@Test
	public void testGenerateFileResponseComparator() {
		Comparator<FileResponse> fileResponseComparator = metadataResponseComparatorFactory.generateFileResponseComparator(SortType.USER_ARRANGE);
		Assert.assertNotNull(fileResponseComparator);
	}

}
