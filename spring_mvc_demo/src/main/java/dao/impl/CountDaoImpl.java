package dao.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import model.BaseMetadata;
import model.CountNode;
import model.MetadataAttr;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.jdbc.core.namedparam.SqlParameterSourceUtils;
import org.springframework.stereotype.Repository;

import dao.CountDao;

@Repository
public class CountDaoImpl implements CountDao {

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	@Value("${sql.checkFoldersOrFiles}")
	private String checkSql;
	@Value("${sql.addFolders}")
	private String addFolderSql;
	@Value("${sql.addFiles}")
	private String addFileSql;
	@Value("${sql.deleteFoldersOrFiles}")
	private String deleteSql;
	@Value("${sql.queryCount}")
	private String querySql;
	@Value("${sql.updateParent}")
	private String updateSql;
	@Value("${sql.updateFolder}")
	private String updateFolderSql;
	@Value("${sql.findNotLeaf}")
	private String findNotLeafSql;
	@Value("${sql.findNotSingleLeaf}")
	private String findNotSingleLeafSql;
	@Value("${sql.updateNotSingleLeafParentFolderForAdd}")
	private String updateNotSingleSql;
	@Value("${sql.updateSingleLeafParentFolderForAdd}")
	private String updateSingleSql;
	@Value("${sql.updateNodeFolders}")
	private String updateNodeFoldersSql;
	@Value("${sql.getNode}")
	private String getNodeSql;
	@Value("${sql.getBackUpNodeByOriginId}")
	private String getBackUpNodeByOriginId;
	@Value("${sql.getBackUpNode}")
	private String getBackUpNode;

	// bean row mapper
	private BeanPropertyRowMapper<MetadataAttr> metadataAttrBeanRowMapper = new BeanPropertyRowMapper<MetadataAttr>(MetadataAttr.class);

	@Override
	public List<CountNode> addFolders(List<CountNode> counts) {
		SqlParameterSource[] params = SqlParameterSourceUtils
				.createBatch(counts.toArray());
		jdbcTemplate.batchUpdate(addFolderSql, params);
		return counts;
	}

	@Override
	public List<CountNode> addFiles(List<CountNode> counts) {
		SqlParameterSource[] param = SqlParameterSourceUtils.createBatch(counts
				.toArray());
		jdbcTemplate.batchUpdate(addFileSql, param);
		return counts;
	}

	@Override
	public List<CountNode> updateParent(List<CountNode> counts) {
		SqlParameterSource[] param = SqlParameterSourceUtils.createBatch(counts
				.toArray());
		jdbcTemplate.batchUpdate(updateSql, param);
		return counts;
	}

	@Override
	public int delete(List<String> idList) {
		// non checked if empty return code -1 will cause problem
		if (idList.isEmpty()) {
			return 0;
		}
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("leafId", idList);
		return jdbcTemplate.update(deleteSql, param);
	}

	@Override
	public BaseMetadata getNode(String id) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("id", id);
		List<Map<String, Object>> result = jdbcTemplate.queryForList(
				getNodeSql, param);
		BaseMetadata metadata = new BaseMetadata();
		for (Map<String, Object> item : result) {
			metadata.setId(item.get("id").toString());
			metadata.setParentId(item.get("parent_id").toString());
		}
		return metadata;
	}

	@Override
	public BaseMetadata getNodeByOriginId(String id) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("id", id);
		List<Map<String, Object>> result = jdbcTemplate.queryForList(
				getBackUpNodeByOriginId, param);
		BaseMetadata metadata = new BaseMetadata();
		for (Map<String, Object> item : result) {
			metadata.setId(item.get("id").toString());
			metadata.setParentId(item.get("parent_id").toString());
			metadata.setOriginId(item.get("original_index_id").toString());
		}
		return metadata;
	}

	@Override
	public BaseMetadata getBackUpNode(String id) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("id", id);
		List<Map<String, Object>> result = jdbcTemplate.queryForList(
				getBackUpNode, param);
		BaseMetadata metadata = new BaseMetadata();
		for (Map<String, Object> item : result) {
			metadata.setId(item.get("id").toString());
			metadata.setParentId(item.get("parent_id").toString());
			metadata.setOriginId(item.get("original_index_id").toString());
		}
		return metadata;
	}

	@Override
	public MetadataAttr queryCount(String id) {
		return jdbcTemplate.queryForObject(querySql, new MapSqlParameterSource("id", id), metadataAttrBeanRowMapper);
	}

	public ArrayList<CountNode> checkExist(List<String> idList) {
		if (idList.isEmpty()) {
			return null;
		}
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("fId", idList);
		List<Map<String, Object>> result = jdbcTemplate.queryForList(checkSql,
				param);
		ArrayList<CountNode> lst = new ArrayList<CountNode>();
		for (Map<String, Object> item : result) {
			CountNode count = new CountNode();
			count.setLeafId(item.get("metadata_index_id").toString());
			count.setLeafSize(Long.parseLong(item.get("total_size").toString()));
			count.setLeafFolderCount(Integer.parseInt(item.get("folders_count")
					.toString()));
			lst.add(count);
		}
		return lst;
	}

	@Override
	public List<CountNode> updateParentFolder(List<CountNode> counts) {
		if (counts.isEmpty()) {
			return null;
		}
		SqlParameterSource[] param = SqlParameterSourceUtils.createBatch(counts
				.toArray());
		jdbcTemplate.batchUpdate(updateFolderSql, param);
		return counts;
	}

	@Override
	public List<CountNode> findNotLeaf(List<String> idList) {
		if (idList.isEmpty()) {
			return null;
		}
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("leafId", idList);
		List<CountNode> lst = new ArrayList<CountNode>();
		List<Map<String, Object>> result = jdbcTemplate.queryForList(
				findNotLeafSql, param);
		for (Map<String, Object> item : result) {
			CountNode node = new CountNode();
			node.setLeafId(item.get("parent_id").toString());
			lst.add(node);
		}
		return lst;
	}

	@Override
	public List<CountNode> findNotSingleLeaf(List<String> idList) {
		if (idList.isEmpty()) {
			return null;
		}
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("leafId", idList);
		List<CountNode> lst = new ArrayList<CountNode>();
		List<Map<String, Object>> result = jdbcTemplate.queryForList(
				findNotSingleLeafSql, param);
		for (Map<String, Object> item : result) {
			CountNode node = new CountNode();
			node.setLeafId(item.get("id").toString());
			node.setFatherId(item.get("parent_id").toString());
			lst.add(node);
		}
		return lst;
	}

	@Override
	public List<CountNode> updateSingleLeafParents(List<CountNode> countNodes) {
		if (countNodes.isEmpty()) {
			return null;
		}
		SqlParameterSource[] param = SqlParameterSourceUtils
				.createBatch(countNodes.toArray());
		jdbcTemplate.batchUpdate(updateSingleSql, param);
		return countNodes;
	}

	@Override
	public List<CountNode> updateNotSingleLeafParents(List<CountNode> countNodes) {
		if (countNodes.isEmpty()) {
			return null;
		}
		SqlParameterSource[] param = SqlParameterSourceUtils
				.createBatch(countNodes.toArray());
		jdbcTemplate.batchUpdate(updateNotSingleSql, param);
		return countNodes;
	}

	@Override
	public List<String> updateNodeFolder(List<String> idList) {
		if (idList.isEmpty()) {
			return null;
		}
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("leafId", idList);
		jdbcTemplate.update(updateNodeFoldersSql, param);
		return idList;
	}
}
