package dao.impl;

import java.sql.Types;
import java.util.List;

import model.BaseMetadata;
import model.MetadataAttr;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.BeanPropertySqlParameterSource;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSourceUtils;
import org.springframework.stereotype.Repository;

import dao.MetadataAttrDao;

@Repository
public class MetadataAttrDaoImpl implements MetadataAttrDao {

	@Value("${sql.create_metadata_attr}")
	private String createMetadataAttrSql;

	@Value("${sql.update_metadata_attr_on_origin}")
	private String updateMetadataAttrOnOriginSql;

	@Value("${sql.delete_metadata_attr}")
	private String deleteMetadataAttrSql;

	@Value("${sql.delete_metadata_attrs}")
	private String deleteMetadataAttrsSql;

	@Value("${sql.update_metadata_attr_on_origin_with_aspect}")
	private String updateMetadataAttrOnOriginWithAspectSql;

	@Value("${sql.get_metadata_attr}")
	private String getMetadataAttrSql;

	@Value("${sql.update_metadata_attr}")
	private String updateMetadataAttrSql;

	@Value("${sql.batch_update_attr_count_size}")
	private String batchUpdateCountSize;

	@Value("${sql.update_root_size}")
	private String updateRootSize;

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	// bean row mapper
	private BeanPropertyRowMapper<MetadataAttr> metadataAttrBeanRowMapper = new BeanPropertyRowMapper<MetadataAttr>(
			MetadataAttr.class);

	@Override
	public void updateRootSize(String rootId, long size) {

		jdbcTemplate.update(
				updateRootSize,
				new MapSqlParameterSource().addValue("rootId", rootId,
						Types.CHAR).addValue("totalSize", size, Types.BIGINT));
	}

	@Override
	public MetadataAttr createMetadataAttr(MetadataAttr attr) {
		jdbcTemplate.update(createMetadataAttrSql,
				new BeanPropertySqlParameterSource(attr));
		return attr;
	}

	@Override
	public List<MetadataAttr> batchCreateMetadataAttr(List<MetadataAttr> attrs) {
		batchUpdate(createMetadataAttrSql, attrs);
		return attrs;
	}

	@Override
	public MetadataAttr updateMetadataAttrOnOrigin(MetadataAttr attr) {
		jdbcTemplate.update(updateMetadataAttrOnOriginSql,
				new BeanPropertySqlParameterSource(attr));
		return attr;
	}

	@Override
	public List<MetadataAttr> batchUpdateMetadataAttrOnOrigin(
			List<MetadataAttr> attrs) {
		batchUpdate(updateMetadataAttrOnOriginSql, attrs);
		return attrs;
	}

	@Override
	public void deleteMetadataAttr(String matedataIndexId) {
		jdbcTemplate.update(deleteMetadataAttrSql, new MapSqlParameterSource(
				"metadataIndexId", matedataIndexId));
	}

	@Override
	public void deleteMetadataAttrs(List<String> matedataIndexIds) {
		if (matedataIndexIds.isEmpty()) {
			return;
		}
		jdbcTemplate.update(deleteMetadataAttrsSql, new MapSqlParameterSource(
				"matedataIndexIds", matedataIndexIds));
	}

	private int[] batchUpdate(String sql, List<MetadataAttr> attrs) {
		return jdbcTemplate.batchUpdate(sql,
				SqlParameterSourceUtils.createBatch(attrs.toArray()));
	}

	@Override
	public MetadataAttr updateMetadataAttrOnOriginWithAspec(MetadataAttr attr) {
		jdbcTemplate.update(updateMetadataAttrOnOriginWithAspectSql,
				new BeanPropertySqlParameterSource(attr));
		return attr;
	}

	@Override
	public List<MetadataAttr> batchUpdateMetadataAttrOnOriginWithAspect(
			List<MetadataAttr> attrs) {
		batchUpdate(updateMetadataAttrOnOriginWithAspectSql, attrs);
		return attrs;
	}

	@Override
	public MetadataAttr getMetadataAttr(String metadataIndexId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("metadataIndexId", metadataIndexId);
		return jdbcTemplate.queryForObject(getMetadataAttrSql, parameterSource,
				metadataAttrBeanRowMapper);
	}

	@Override
	public void updateMetadataAttr(MetadataAttr attr) {
		jdbcTemplate.update(updateMetadataAttrSql,
				new BeanPropertySqlParameterSource(attr));
	}

	@Override
	public void batchUpdateSizeCount(List<BaseMetadata> metadatas) {
		jdbcTemplate.batchUpdate(batchUpdateCountSize,
				SqlParameterSourceUtils.createBatch(metadatas.toArray()));
	}

}
