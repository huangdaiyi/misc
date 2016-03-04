package dao.impl;

import java.sql.Types;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import model.GlobalIcon;
import model.GlobalIconGroup;
import model.Metadata;
import model.MobileFolders;
import model.response.GetSubBackupCountResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.BeanPropertySqlParameterSource;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.jdbc.core.namedparam.SqlParameterSourceUtils;
import org.springframework.stereotype.Repository;

import utils.StringUtils;
import constants.SyncType;
import dao.FolderDao;

@Repository
public class FolderDaoImpl implements FolderDao {

	@Value("${sql.get_folder_id_by_criteria}")
	private String getFolderIdByCriteriaSql;
	@Value("${sql.set_folder_linked}")
	private String setFolderLinkedSql;
	@Value("${sql.set_folder_unlinked}")
	private String setFolderUnlinkedSql;
	@Value("${sql.query_folder}")
	private String queryFolderSql;
	@Value("${sql.delete_mobile_folder_by_ids}")
	private String deleteMobileFolderByIdsSql;
	@Value("${sql.add_global_icon}")
	private String addGlobalIconSql;
	@Value("${sql.delete_global_icon}")
	private String deleteGlobalIconSql;
	@Value("${sql.get_global_icon_by_user}")
	private String getGlobalIconByUserSql;
	@Value("${sql.get_global_icon_group_id}")
	private String getGlobalIconGroupIdSql;
	@Value("${sql.get_global_icon}")
	private String getGlobalIconSql;
	@Value("${sql.move_global_icon}")
	private String moveGlobalIconSql;
	@Value("${sql.get_global_icon_group}")
	private String getGlobalIconGroupSql;
	@Value("${sql.create_global_icon_group}")
	private String createGlobalIconGroupSql;
	@Value("${sql.delete_global_icon_group}")
	private String deleteGlobalIconGroupSql;
	@Value("${sql.update_global_icon_group}")
	private String updateGlobalIconGroupSql;
	@Value("${sql.update_priority}")
	String updatePrioritySql;
	@Value("${sql.get_subpath_id}")
	private String getSubElementIdSql;
	@Value("${sql.get_sub_folder_backup_count}")
	private String getSubBackupFolderCountSql;
	@Value("${sql.get_folder_by_sharedId}")
	private String getFolderBySharedId;

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	private BeanPropertyRowMapper<Metadata> metadataBeanRowMapper = new BeanPropertyRowMapper<Metadata>(
			Metadata.class);
	// bean row mapper
	private BeanPropertyRowMapper<GlobalIcon> iconBeanRowMapper = new BeanPropertyRowMapper<GlobalIcon>(
			GlobalIcon.class);
	private BeanPropertyRowMapper<GlobalIconGroup> iconGroupBeanRowMapper = new BeanPropertyRowMapper<GlobalIconGroup>(
			GlobalIconGroup.class);

	@Override
	public String getFolderIdByCriteria(String parentId, String name,
			String deviceUid, String fullSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("parentId", parentId);
		paramSource.addValue("name", name);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("fullSourcePath", fullSourcePath);
		try {
			return jdbcTemplate.queryForObject(getFolderIdByCriteriaSql,
					paramSource, String.class);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public void setFolderLinked(String user, String fullSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("user", user);
		paramSource.addValue("fullSourcePath", fullSourcePath);
		jdbcTemplate.update(setFolderLinkedSql, paramSource);
	}

	@Override
	public void setFolderUnlinked(String user, String fullSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("user", user);
		paramSource.addValue("fullSourcePath", fullSourcePath);
		jdbcTemplate.update(setFolderUnlinkedSql, paramSource);
	}

	@Override
	/**
	 * type = null means both of sync & unsync
	 */
	public List<MobileFolders> getQueryFolders(String user, SyncType type,
			String fullSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("user", user);
		paramSource.addValue("type", type.toString());
		paramSource.addValue("fullSourcePath", fullSourcePath);

		List<MobileFolders> result = jdbcTemplate.query(queryFolderSql,
				paramSource, MobileFolders.getMapper());

		return result;
	}

	@Override
	public void deleteMobileFolderByIds(List<Integer> idList) {
		if (idList.isEmpty()) {
			return;
		}
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("idList", idList);
		jdbcTemplate.update(deleteMobileFolderByIdsSql, paramSource);
	}

	public void addGlobalIcon(List<GlobalIcon> globalIcons) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(globalIcons.toArray());
		jdbcTemplate.batchUpdate(addGlobalIconSql, batch);
	}

	@Override
	public void deleteGlobalIcon(List<GlobalIcon> globalIcons) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(globalIcons.toArray());
		jdbcTemplate.batchUpdate(deleteGlobalIconSql, batch);
	}

	@Override
	public List<GlobalIcon> getGlobalIconByUser(String userId) {
		return jdbcTemplate.query(getGlobalIconByUserSql,
				new MapSqlParameterSource().addValue("user_id", userId),
				iconBeanRowMapper);
	}

	@Override
	public Integer getGlobalIconGroupId(String userId, boolean isFolder,
			String groupName) {
		try {
			return jdbcTemplate.queryForObject(
					getGlobalIconGroupIdSql,
					new MapSqlParameterSource()
							.addValue("user_id", userId, Types.VARCHAR)
							.addValue("is_folder", isFolder, Types.BOOLEAN)
							.addValue("group_name", groupName), Integer.class);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public List<GlobalIcon> getGlobalIcon(String userId,
			boolean includeUserDefinedIcon, boolean isFolder, int groupId) {
		return jdbcTemplate.query(
				getGlobalIconSql,
				new MapSqlParameterSource()
						.addValue("user_id", userId, Types.VARCHAR)
						.addValue("include_user_defined_icon",
								includeUserDefinedIcon, Types.BOOLEAN)
						.addValue("is_folder", isFolder, Types.BOOLEAN)
						.addValue("group_id", groupId), iconBeanRowMapper);
	}

	@Override
	public void moveGlobalIcon(int iconId, int destinationGroupId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("destinationGroupId", destinationGroupId);
		paramSource.addValue("iconId", iconId);
		jdbcTemplate.update(moveGlobalIconSql, paramSource);
	}

	@Override
	public void createGlobalIconGroup(String userId, String name,
			boolean isFolder) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("userId", userId);
		paramSource.addValue("name", name);
		paramSource.addValue("isFolder", isFolder);
		jdbcTemplate.update(createGlobalIconGroupSql, paramSource);
	}

	@Override
	public void deleteGlobalIconGroup(int groupId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("groupId", groupId);
		jdbcTemplate.update(deleteGlobalIconGroupSql, paramSource);
	}

	@Override
	public void updateGlobalIconGroup(GlobalIconGroup globalIconGroup) {
		jdbcTemplate.update(updateGlobalIconGroupSql,
				new BeanPropertySqlParameterSource(globalIconGroup));
	}

	@Override
	public List<GlobalIconGroup> getGlobalIconGroup(String userId,
			boolean isFolder) {
		return jdbcTemplate.query(
				getGlobalIconGroupSql,
				new MapSqlParameterSource().addValue("user_id", userId,
						Types.VARCHAR).addValue("is_folder", isFolder,
						Types.BOOLEAN), iconGroupBeanRowMapper);
	}

	@Override
	public void updatePriority(Metadata metadata) {
		jdbcTemplate.update(updatePrioritySql,
				new BeanPropertySqlParameterSource(metadata));
	}

	@Override
	public List<GetSubBackupCountResponse> getSubBackupCount(List<String> idList) {
		if (idList.isEmpty()) {
			return null;
		}
		List<GetSubBackupCountResponse> list = new ArrayList<GetSubBackupCountResponse>();
		MapSqlParameterSource map = new MapSqlParameterSource();
		map.addValue("id", idList);
		List<Map<String, Object>> result = jdbcTemplate.queryForList(
				getSubBackupFolderCountSql, map);
		for (Map<String, Object> item : result) {
			GetSubBackupCountResponse entity = new GetSubBackupCountResponse();
			entity.setBackupCount(Integer.parseInt(item.get("backup_count")
					.toString()));
			entity.setName(item.get("name").toString());
			entity.setSourcePath(item.get("source_path").toString());
			list.add(entity);
		}
		return list;
	}

	@Override
	public List<String> getSubElementId(String id, String name, String owerId) {
		MapSqlParameterSource map = new MapSqlParameterSource();
		map.addValue("id", id);
		map.addValue("name", name);
		if (!StringUtils.isNullOrEmpty(owerId)) {
			getSubElementIdSql = getSubElementIdSql.concat(" AND owner_id='"
					+ owerId + "'");
		}
		List<Map<String, Object>> result = jdbcTemplate.queryForList(
				getSubElementIdSql, map);
		List<String> list = new ArrayList<String>();
		for (Map<String, Object> item : result) {
			list.add(item.get("id").toString());
		}
		return list;
	}

	@Override
	public List<Metadata> getBySharedRootId(String sharedRootId) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("shared_root_id", sharedRootId);
		return jdbcTemplate.query(getFolderBySharedId, param,
				metadataBeanRowMapper);
	}
}
