package service.impl;

import java.util.ArrayList;
import java.util.List;

import model.MetadataAttr;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import utils.StringUtils;
import constants.HttpStatus;
import dao.MetadataAttrDao;
import exception.MetadataException;

public class MetadataAttrServiceImplTest {

	@Mock
	private MetadataAttrDao metadataAttrDao;

	@InjectMocks
	private MetadataAttrServiceImpl impl;

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);

	}

	@Test
	public void createMetadataAttrTest() {
		String id = StringUtils.getUUID();
		MetadataAttr attr = Mockito
				.mock(MetadataAttr.class);
		Mockito.when(attr.getMetadataIndexId()).thenReturn(id);
		Mockito.when(metadataAttrDao.createMetadataAttr(attr)).thenReturn(attr);
		MetadataAttr result = impl.createMetadataAttr(attr);
		Assert.assertEquals(attr, result);
		Assert.assertEquals(id, result.getMetadataIndexId());

	}

	@Test
	public void createMetadataAttrBadRequestTest() {
		MetadataAttr attr = Mockito
				.mock(MetadataAttr.class);
		Mockito.when(metadataAttrDao.createMetadataAttr(attr)).thenReturn(attr);
		try {
			impl.createMetadataAttr(attr);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS, e.getHttpStatus());
		}

	}

	@Test
	public void metadataAttrBadRequestTest() {
		String matedataIndexId = "test id";
		impl.deleteMetadataAttr(matedataIndexId);
	}

	@Test
	public void metadataAttrTest() {
		String matedataIndexId = "";
		try {
			impl.deleteMetadataAttr(matedataIndexId);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS, e.getHttpStatus());
		}
	}

	@Test
	public void updateMetadataOnOriginTest() {

		String id = StringUtils.getUUID();
		MetadataAttr attr = Mockito
				.mock(MetadataAttr.class);
		Mockito.when(attr.getMetadataIndexId()).thenReturn(id);
		Mockito.when(metadataAttrDao.updateMetadataAttrOnOrigin(attr)).thenReturn(
				attr);
		MetadataAttr result = impl.updateMetadataAttrOnOrigin(attr);
		Assert.assertEquals(attr, result);
		Assert.assertEquals(id, result.getMetadataIndexId());

	}

	@Test
	public void updateMetadataOnOriginBadRequestTest() {
		MetadataAttr attr = Mockito
				.mock(MetadataAttr.class);
		Mockito.when(metadataAttrDao.updateMetadataAttrOnOrigin(attr)).thenReturn(
				attr);
		try {
			impl.updateMetadataAttrOnOrigin(attr);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS, e.getHttpStatus());
		}

	}

	@Test
	public void batchcreateMetadataAttrTest() {
		String id = StringUtils.getUUID();
		MetadataAttr attr = Mockito
				.mock(MetadataAttr.class);
		Mockito.when(attr.getMetadataIndexId()).thenReturn(id);
		List<MetadataAttr> attrs = new ArrayList<MetadataAttr>();
		attrs.add(attr);

		String id2 = StringUtils.getUUID();
		MetadataAttr attr2 = Mockito
				.mock(MetadataAttr.class);
		Mockito.when(attr2.getMetadataIndexId()).thenReturn(id2);
		attrs.add(attr2);

		Mockito.when(metadataAttrDao.batchCreateMetadataAttr(attrs))
				.thenReturn(attrs);
		List<MetadataAttr> result = impl
				.batchCreateMetadataAttr(attrs);
		Assert.assertEquals(id, result.get(0).getMetadataIndexId());
		Assert.assertEquals(id2, result.get(1).getMetadataIndexId());
	}

	@Test
	public void batchUpdateMetadataOnOriginTest() {
		String id = StringUtils.getUUID();
		MetadataAttr attr = Mockito
				.mock(MetadataAttr.class);
		Mockito.when(attr.getMetadataIndexId()).thenReturn(id);
		List<MetadataAttr> attrs = new ArrayList<MetadataAttr>();
		attrs.add(attr);

		String id2 = StringUtils.getUUID();
		MetadataAttr attr2 = Mockito
				.mock(MetadataAttr.class);
		Mockito.when(attr2.getMetadataIndexId()).thenReturn(id2);
		attrs.add(attr2);
		//int[] affectRows = {1,1,1,1};
		Mockito.when(metadataAttrDao.batchUpdateMetadataAttrOnOrigin(attrs))
				.thenReturn(attrs);
		List<MetadataAttr> result = impl
				.batchUpdateMetadataAttrOnOrigin(attrs);
		Assert.assertEquals(id, result.get(0).getMetadataIndexId());
		Assert.assertEquals(id2, result.get(1).getMetadataIndexId());
	}

	@Test
	public void batchDeleteTest() {
		List<String> matedataIndexIds = new ArrayList<String>();
		matedataIndexIds.add("test id 1");
		matedataIndexIds.add("test id 2");
		impl.deleteMetadataAttrs(matedataIndexIds);

	}

	@Test
	public void batchDeleteBadRequestTest() {
		List<String> matedataIndexIds = null;
		try {
			impl.deleteMetadataAttrs(matedataIndexIds);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS, e.getHttpStatus());
		}
		
		matedataIndexIds = new ArrayList<String>();
		
		try {
			impl.deleteMetadataAttrs(matedataIndexIds);
		} catch (MetadataException e) {
			Assert.assertEquals(HttpStatus.ERROR_IN_PARAMETERS, e.getHttpStatus());
		}
	}

}
