package dao.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import model.BackupMetadata;
import model.BaseMetadata;
import model.Metadata;
import model.UserSetting;
import model.request.RestCommonRequest;
import model.response.GetUserResponse;

import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.namedparam.BeanPropertySqlParameterSource;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;

import utils.StringUtils;
import dao.BackupMetadataDao;
import dao.MetadataDao;
import dao.UserDao;

@Repository
public class UserDaoImpl implements UserDao {

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;
	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private BackupMetadataDao ackupMetadataDao;

	@Value("${sql.user_usage}")
	private String userUsageSql;
	@Value("${sql.setting_reset}")
	private String settingUpdateSql;
	@Value("${sql.create_user}")
	private String createUserSql;
	@Value("${sql.update_collaborate}")
	private String updateCollaboSql;
	@Value("${sql.get_user_allnode}")
	private String getUserAllNodeSql;
	@Value("${sql.delete_all_mobile_folders}")
	private String deleteAllMobileFolderSql;
	@Value("${sql.delete_mobile_folders}")
	private String deleteMobileFolderSql;
	@Value("${sql.get_common_folder_childfiles}")
	private String getCommonFolderChildSql;
	@Value("${sql.get_MyDocment_all_childfiles}")
	private String getMyDocumentFileSql;
	@Value("${sql.get_all_common_folder_childfiles}")
	private String getAllCommonFolderFilesSql;
	@Value("${sql.delete_all_backupdata}")
	private String deleteAllBackupDataSql;
	@Value("${sql.delete_backup_in_attr}")
	private String deleteBackupInAttrSql;
	@Value("${sql.get_user_subNodes}")
	private String getUsersubNodes;
	@Value("${sql.update_user_disk_space}")
	private String updateUserDiskSpaceSql;

	@Override
	public int updateSetting(UserSetting userSetting) {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("settings", userSetting.getSettings());
		map.put("user", userSetting.getUser());
		return jdbcTemplate.update(settingUpdateSql, map);
	}

	@Override
	public int createUser(GetUserResponse user) {
		return jdbcTemplate.update(createUserSql,
				new BeanPropertySqlParameterSource(user));
	}

	@Override
	public GetUserResponse getUser(String userId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("UserId", userId);
		try {
			return jdbcTemplate.queryForObject(userUsageSql, parameterSource,
					GetUserResponse.getMapper());
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	public List<Metadata> getSubLevel(String owner_id, List<String> parentId,
			List<String> name, boolean isBackUp) {
		
		List<Metadata> list = null;
		
		if(isBackUp) {
			List<BackupMetadata> backups = ackupMetadataDao.getSubBackupByParentsAndNames(owner_id, parentId, name);
			list = new ArrayList<Metadata>();
			for(BackupMetadata backup : backups) {
				Metadata metadata = new Metadata();
				metadata.fromBaseMetadata(backup);
				list.add(metadata);
			}
		}
		else {
			list = metadataDao.batchGetMetadata(owner_id, parentId, name);
		}
		
		return list;
	}
	
	
	public List<BaseMetadata> getUserAllNode(String leafId) {

		List<BaseMetadata> lst = new ArrayList<BaseMetadata>();
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("id", leafId);
		List<Map<String, Object>> rst = jdbcTemplate.queryForList(
				getUserAllNodeSql, param);
		for (Map<String, Object> item : rst) {
			BaseMetadata entity = new BaseMetadata();
			entity.setId(item.get("id").toString());
			entity.setParentId(item.get("parent_id").toString());
			lst.add(entity);
		}
		return lst;
	}

	@Override
	public int updateCollaborate(List<String> idList) {
		// non checked if empty return -1 will cause problem
		if (idList.isEmpty()) {
			return 0;
		}
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("metadata_index_id", idList);
		return jdbcTemplate.update(updateCollaboSql, param);
	}

	@Override
	public int deleteAllMobileFolders(String user) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("user", user);
		return jdbcTemplate.update(deleteAllMobileFolderSql, param);
	}

	@Override
	public int deleteMobileFolders(String user, String deviceUid) {
		MapSqlParameterSource map = new MapSqlParameterSource();
		map.addValue("user", user);
		map.addValue("device_uid", deviceUid);
		return jdbcTemplate.update(deleteMobileFolderSql, map);
	}

	@Override
	public List<Metadata> getOridinaryCommonFolderChild(
			RestCommonRequest request) {
		String newSQL = "";
		if (!StringUtils.isNullOrEmpty(request.getDeviceUid())) {
			newSQL = getCommonFolderChildSql.concat(" AND device_uid='"
					+ request.getDeviceUid() + "'");
		} else {
			newSQL = getCommonFolderChildSql;
		}
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("owner_id", request.getUserId());
		param.addValue("name", FilenameUtils.getName(request.getPath()));
		List<Map<String, Object>> result = jdbcTemplate.queryForList(newSQL,
				param);
		List<Metadata> list = new ArrayList<Metadata>();
		for (Map<String, Object> item : result) {
			Metadata entity = new Metadata();
			entity.setId(item.get("id").toString());
			entity.setParentId(item.get("parent_id").toString());
			entity.setName(item.get("name").toString());
			entity.setFolder(Boolean.parseBoolean(item.get("is_folder")
					.toString()));
			entity.setSize(Long.parseLong(item.get("size").toString()));
			entity.setTotalSize(entity.getSize());
			entity.setDeviceUid(item.get("device_uid").toString());
			entity.setOwnerId(item.get("owner_id").toString());
			list.add(entity);
		}
		return list;
	}

	@Override
	public List<Metadata> getMyDocumentFileChild(RestCommonRequest request) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("owner_id", request.getUserId());
		String newSQL = "";
		if (!StringUtils.isNullOrEmpty(request.getDeviceUid())) {
			newSQL = getMyDocumentFileSql.concat(" AND device_uid='"
					+ request.getDeviceUid() + "'");
		} else {
			newSQL = getMyDocumentFileSql;
		}
		List<Map<String, Object>> result = jdbcTemplate.queryForList(newSQL,
				param);
		List<Metadata> list = new ArrayList<Metadata>();
		for (Map<String, Object> item : result) {
			Metadata entity = new Metadata();
			entity.setId(item.get("id").toString());
			entity.setParentId(item.get("parent_id").toString());
			entity.setName(item.get("name").toString());
			entity.setFolder(Boolean.parseBoolean(item.get("is_folder")
					.toString()));
			entity.setTotalSize(Long.parseLong(item.get("size").toString()));
			entity.setDeviceUid(item.get("device_uid").toString());
			entity.setOwnerId(item.get("owner_id").toString());
			list.add(entity);
		}
		return list;
	}

	@Override
	public List<Metadata> getAllCommonFiles(RestCommonRequest request) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("owner_id", request.getUserId());
		String newSQL = "";
		if (!StringUtils.isNullOrEmpty(request.getDeviceUid())) {
			newSQL = getAllCommonFolderFilesSql.concat(" AND device_uid='"
					+ request.getDeviceUid() + "'");
		} else {
			newSQL = getAllCommonFolderFilesSql;
		}
		List<Map<String, Object>> result = jdbcTemplate.queryForList(newSQL,
				param);
		List<Metadata> list = new ArrayList<Metadata>();
		for (Map<String, Object> item : result) {
			Metadata entity = new Metadata();
			entity.setId(item.get("id").toString());
			entity.setParentId(item.get("parent_id").toString());
			entity.setName(item.get("name").toString());
			entity.setFolder(Boolean.parseBoolean(item.get("is_folder")
					.toString()));
			entity.setTotalSize(Long.parseLong(item.get("size").toString()));
			entity.setDeviceUid(item.get("device_uid").toString());
			entity.setOwnerId(item.get("owner_id").toString());
			list.add(entity);
		}
		return list;
	}

	@Override
	public int deleteAllBackupData(String userId) {
		MapSqlParameterSource map = new MapSqlParameterSource();
		map.addValue("owner_id", userId);
		return jdbcTemplate.update(deleteAllBackupDataSql, map);
	}

	@Override
	public int deleteBackupInAttr(String userId) {
		MapSqlParameterSource map = new MapSqlParameterSource();
		map.addValue("owner_id", userId);
		return jdbcTemplate.update(deleteBackupInAttrSql, map);
	}

	@Override
	public void updateUserDiskSpace(String userId, long total) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("userId", userId);
		param.addValue("total", total);
		jdbcTemplate.update(updateUserDiskSpaceSql, param);
	}

}