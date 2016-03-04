package dao;

import java.util.List;

import model.BaseMetadata;
import model.MetadataAttr;

public interface MetadataAttrDao {

	public MetadataAttr createMetadataAttr(MetadataAttr attr);

	public List<MetadataAttr> batchCreateMetadataAttr(List<MetadataAttr> attrs);

	public MetadataAttr updateMetadataAttrOnOrigin(MetadataAttr attr);

	public List<MetadataAttr> batchUpdateMetadataAttrOnOrigin(
			List<MetadataAttr> attrs);

	public void deleteMetadataAttr(String matedataIndexId);

	public void deleteMetadataAttrs(List<String> matedataIndexIds);

	public MetadataAttr updateMetadataAttrOnOriginWithAspec(MetadataAttr attr);

	public List<MetadataAttr> batchUpdateMetadataAttrOnOriginWithAspect(
			List<MetadataAttr> attrs);

	public MetadataAttr getMetadataAttr(String matedataIndexId);

	public void updateMetadataAttr(MetadataAttr attr);

	public void batchUpdateSizeCount(List<BaseMetadata> metadatas);

	public void updateRootSize(String rootId, long size);

}