package service.impl;

import java.util.List;

import model.MetadataAttr;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import service.MetadataAttrService;
import utils.StringUtils;
import constants.HttpStatus;
import dao.MetadataAttrDao;
import exception.MetadataException;

@Service
public class MetadataAttrServiceImpl implements MetadataAttrService {

	@Autowired
	private MetadataAttrDao metadataAttrDao;

	@Override
	public MetadataAttr createMetadataAttr(MetadataAttr attr) {

		if (attr == null
				|| StringUtils.isNullOrEmpty(attr.getMetadataIndexId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);

		}
		return metadataAttrDao.createMetadataAttr(attr);
	}

	@Override
	public List<MetadataAttr> batchCreateMetadataAttr(
			List<MetadataAttr> attrs) {

		
		if (StringUtils.isNullOrEmpty(attrs)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return metadataAttrDao.batchCreateMetadataAttr(attrs);
	}

	@Override
	public MetadataAttr updateMetadataAttrOnOrigin(
			MetadataAttr attr) {

		if (attr == null
				|| StringUtils.isNullOrEmpty(attr.getMetadataIndexId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);

		}
		return metadataAttrDao.updateMetadataAttrOnOrigin(attr);
	}

	@Override
	public List<MetadataAttr> batchUpdateMetadataAttrOnOrigin(
			List<MetadataAttr> attrs) {

		if (StringUtils.isNullOrEmpty(attrs)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		metadataAttrDao.batchUpdateMetadataAttrOnOrigin(attrs);
		// TODO:
		return attrs;
	}

	@Override
	public void deleteMetadataAttr(String matedataIndexId) {

		if (StringUtils.isNullOrEmpty(matedataIndexId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		metadataAttrDao.deleteMetadataAttr(matedataIndexId);
	}

	@Override
	public void deleteMetadataAttrs(List<String> matedataIndexIds) {

		if (StringUtils.isNullOrEmpty(matedataIndexIds)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		metadataAttrDao.deleteMetadataAttrs(matedataIndexIds);
	}

	@Override
	public MetadataAttr updateMetadataAttrOnOriginWithAspec(
			MetadataAttr attr) {
		if (attr == null
				|| StringUtils.isNullOrEmpty(attr.getMetadataIndexId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		metadataAttrDao.updateMetadataAttrOnOriginWithAspec(attr);
		return attr;
	}

	@Override
	public List<MetadataAttr> batchUpdateMetadataAttrOnOriginWithAspect(
			List<MetadataAttr> attrs) {
		if (StringUtils.isNullOrEmpty(attrs)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		metadataAttrDao.batchUpdateMetadataAttrOnOriginWithAspect(attrs);
		return attrs;
	}

	@Override
	public void updateRootSize(String rootId, long size) {
		if (StringUtils.isNullOrEmpty(rootId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (size == 0) {
			return;
		}
		
		metadataAttrDao.updateRootSize(rootId, size);
		
	}
	 
	
	
}
