package erlang_to_java;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mchange.v2.c3p0.ComboPooledDataSource;

//
// -Xmx2048M
//
public class MigrateErlangToJava {

	private String driverClass = "com.mysql.jdbc.Driver";
	private String jdbcUrl = "jdbc:mysql://127.0.0.1:3306/neweggbox_mobile_dev?max_allowed_packet=2G";
	private String dbUser = "root";
	private String dbPassword = "root";

	private NamedParameterJdbcTemplate jdbcTemplate;
	private ObjectMapper objectMapper = new ObjectMapper();
	private BeanPropertyRowMapper<MetadataIndex> metadataIndexMapper = new BeanPropertyRowMapper<MetadataIndex>(MetadataIndex.class);
	private BeanPropertyRowMapper<ShareLink> shareLinkMapper = new BeanPropertyRowMapper<ShareLink>(ShareLink.class);
	private Map<String, String> metadataIndexMapping;
	private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");

	public MigrateErlangToJava() throws Exception {
		super();
		ComboPooledDataSource dataSource = new ComboPooledDataSource();
		dataSource.setDriverClass(driverClass);
		dataSource.setJdbcUrl(jdbcUrl);
		dataSource.setUser(dbUser);
		dataSource.setPassword(dbPassword);
		dataSource.setMaxPoolSize(1000);
		jdbcTemplate = new NamedParameterJdbcTemplate(dataSource);

		/*
		 * --------------------------------------------------
		 * 
		 * migrate start
		 * 
		 * --------------------------------------------------
		 */
		runDBScript(IOUtils.toString(this.getClass().getResourceAsStream("migrate_erlang_to_java.sql")));

		migrateDeviceInfo();

		migrateMetadataIndexAndBackupMetadataIndexAndAdditionalAttrs();
		migrateCollaborate();
		migrateCollaborateMember();
		migrateReadHistory();
		migrateFileProcessingStatus();
		migrateReaderFileMetadata();
		migrateReaderFileMetadataDetail();
		migrateShareLink();
		migrateTblProfile();
		migrateTextViewSetting();

		migrateUserInfo();
	}

	public static void main(String[] args) throws Exception {
		new MigrateErlangToJava();
	}

	/*
	 * --------------------------------------------------
	 * 
	 * private function
	 * 
	 * --------------------------------------------------
	 */
	private void runDBScript(String source) {
		for (String sql : source.split(";")) {
			sql = sql.trim();
			if (sql.isEmpty()) {
				continue;
			}
			try {
				jdbcTemplate.update(sql, new MapSqlParameterSource());
			} catch (Exception e) {
				System.err.println(e.getMessage());
			}
		}
	}

	private List<String> fetchMetadataIndexIdByParentId(String metadataIndexId, List<String> ignoreMetadataIndexIds, List<String> result) {
		if (ignoreMetadataIndexIds == null || ignoreMetadataIndexIds.contains(metadataIndexId) == false) {
			result.add(metadataIndexId);
		}
		String additionalCondition = "";
		if (ignoreMetadataIndexIds != null && ignoreMetadataIndexIds.isEmpty() == false) {
			additionalCondition = "AND mi.parent_id NOT IN ( " + toInCause(ignoreMetadataIndexIds) + ")";
		}
		List<String> subResult = jdbcTemplate.queryForList("SELECT mi.id FROM metadata_index mi WHERE mi.parent_id = :metadataIndexId " + additionalCondition, new MapSqlParameterSource("metadataIndexId", metadataIndexId),
				String.class);
		for (String childMetadataIndexId : subResult) {
			result = fetchMetadataIndexIdByParentId(childMetadataIndexId, ignoreMetadataIndexIds, result);
		}
		return result;
	}

	private String generateUUID() {
		return UUID.randomUUID().toString();
	}

	private String toInCause(List<String> subResult) {
		if (subResult.isEmpty()) {
			return "";
		}
		return new StringBuilder().append("'").append(StringUtils.join(subResult, "','")).append("'").toString();
	}

	/*
	 * --------------------------------------------------
	 * 
	 * collaborate
	 * 
	 * --------------------------------------------------
	 */
	private void migrateCollaborate() {
		System.out.println("migrating collaborate...");

		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `collaborate` DROP INDEX `metadata_id_idx`,");
		sb.append("DROP COLUMN `metadata_id`,");
		sb.append("CHANGE COLUMN `metadata_index_id` `metadata_index_id` CHAR(36) NOT NULL");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());

		if (metadataIndexMapping != null) {
			List<String> subResult = jdbcTemplate.queryForList("SELECT metadata_index_id FROM collaborate WHERE char_length(metadata_index_id) != 36", new MapSqlParameterSource(), String.class);
			for (String metadataIndexId : subResult) {
				String newMetadataIndexId = metadataIndexMapping.get(metadataIndexId);
				if (newMetadataIndexId == null) {
					continue;
				}
				jdbcTemplate.update("UPDATE collaborate SET metadata_index_id = :newMetadataIndexId WHERE metadata_index_id = :metadataIndexId", new MapSqlParameterSource().addValue("newMetadataIndexId", newMetadataIndexId)
						.addValue("metadataIndexId", metadataIndexId));
			}
			jdbcTemplate.update("DELETE FROM collaborate WHERE char_length(metadata_index_id) != 36", new MapSqlParameterSource());
		}
	}

	/*
	 * --------------------------------------------------
	 * 
	 * collaborate_member
	 * 
	 * --------------------------------------------------
	 */
	private void migrateCollaborateMember() {
		System.out.println("migrating collaborate_member...");

		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `collaborate_member` DROP COLUMN `photo_blocks`,");
		sb.append("CHANGE COLUMN `metadata_index_id` `metadata_index_id` CHAR(36) NOT NULL");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());

		if (metadataIndexMapping != null) {
			List<String> subResult = jdbcTemplate.queryForList("SELECT metadata_index_id FROM collaborate_member WHERE char_length(metadata_index_id) != 36 GROUP BY metadata_index_id", new MapSqlParameterSource(),
					String.class);
			for (String metadataIndexId : subResult) {
				String newMetadataIndexId = metadataIndexMapping.get(metadataIndexId);
				if (newMetadataIndexId == null) {
					continue;
				}
				jdbcTemplate.update("UPDATE collaborate_member SET metadata_index_id = :newMetadataIndexId WHERE metadata_index_id = :metadataIndexId",
						new MapSqlParameterSource().addValue("newMetadataIndexId", newMetadataIndexId).addValue("metadataIndexId", metadataIndexId));
			}
			jdbcTemplate.update("DELETE FROM collaborate_member WHERE char_length(metadata_index_id) != 36", new MapSqlParameterSource());
		}
	}

	/*
	 * --------------------------------------------------
	 * 
	 * device_info
	 * 
	 * --------------------------------------------------
	 */
	private void migrateDeviceInfo() {
		System.out.println("migrating device_info...");

		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `device_info` DROP COLUMN `Status`,");
		sb.append("DROP COLUMN `google_register_id`,");
		sb.append("DROP COLUMN `Device_Name`,");
		sb.append("CHANGE COLUMN `ID` `id` INT(11) NOT NULL,");
		sb.append("CHANGE COLUMN `UserName` `user_id` VARCHAR(100) NOT NULL,");
		sb.append("CHANGE COLUMN `Device_Id` `device_id` VARCHAR(100) NULL,");
		sb.append("CHANGE COLUMN `Last_Edit_User` `last_edit_user` VARCHAR(100) NOT NULL,");
		sb.append("CHANGE COLUMN `Last_Edit_Time` `last_edit_time` DATETIME NOT NULL,");
		sb.append("CHANGE COLUMN `Unique_Id` `unique_id` VARCHAR(100) NOT NULL,");
		sb.append("COMMENT = ''");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());
	}

	/*
	 * --------------------------------------------------
	 * 
	 * metadata_index & backup_metadata_index & metadata_index_additional_attr
	 * 
	 * --------------------------------------------------
	 */
	private void migrateMetadataIndexAndBackupMetadataIndexAndAdditionalAttrs() {
		System.out.println("migrating metadata_index & backup_metadata_index...");

		jdbcTemplate.update("UPDATE `metadata_index` SET `note` = '' WHERE `note` IS NULL", new MapSqlParameterSource());
		jdbcTemplate.update("UPDATE `metadata_index` SET `params` = '' WHERE `params` IS NULL OR `params` = '[]'", new MapSqlParameterSource());
		jdbcTemplate.update("UPDATE `metadata_index` SET `icon_block_id` = '' WHERE `icon_block_id` IS NULL", new MapSqlParameterSource());
		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `metadata_index` DROP COLUMN `encrypted_folder_key`,");
		sb.append("DROP COLUMN `last_edit_time`,");
		sb.append("CHANGE COLUMN `id` `id` CHAR(36) NOT NULL,");
		sb.append("CHANGE COLUMN `parent_id` `parent_id` CHAR(36) NOT NULL DEFAULT '',");
		sb.append("CHANGE COLUMN `name` `name` VARCHAR(200) NOT NULL DEFAULT '',");
		sb.append("CHANGE COLUMN `origin_name` `origin_name` VARCHAR(200) NOT NULL DEFAULT '',");
		sb.append("CHANGE COLUMN `backup_count` `backup_count` INT(11) NOT NULL DEFAULT '-1',");
		sb.append("CHANGE COLUMN `device_uid` `device_uid` VARCHAR(100) NOT NULL DEFAULT '',");
		sb.append("CHANGE COLUMN `link_path` `link_path` VARCHAR(5000) NOT NULL DEFAULT '',");
		sb.append("CHANGE COLUMN `note` `note` VARCHAR(5000) NOT NULL DEFAULT '',");
		sb.append("CHANGE COLUMN `params` `params` VARCHAR(5000) NOT NULL DEFAULT '',");
		sb.append("CHANGE COLUMN `icon_block_id` `icon_block_id` VARCHAR(70) NOT NULL,");
		sb.append("CHANGE COLUMN `original_index_id` `original_index_id` CHAR(36) NULL DEFAULT '',");
		sb.append("ADD COLUMN `modified_at` BIGINT(20) NOT NULL DEFAULT 0 AFTER `origin_name`,");
		sb.append("ADD COLUMN `modified_by` VARCHAR(45) NOT NULL DEFAULT '' AFTER `modified_at`,");
		sb.append("ADD COLUMN `created_at` BIGINT(20) NOT NULL DEFAULT 0 AFTER `is_encrypted`,");
		sb.append("ADD COLUMN `created_by` VARCHAR(45) NOT NULL DEFAULT '' AFTER `created_at`,");
		sb.append("ADD COLUMN `block_id` VARCHAR(96) NOT NULL DEFAULT '' AFTER `created_by`,");
		sb.append("ADD COLUMN `shared_root_id` CHAR(36) NOT NULL DEFAULT '' AFTER `block_id`,");
		sb.append("ADD COLUMN `sync_root_id` CHAR(36) NOT NULL DEFAULT '' AFTER `shared_root_id`,");
		sb.append("ADD COLUMN `type` VARCHAR(20) NOT NULL DEFAULT 'normal' AFTER `sync_root_id`,");
		sb.append("ADD COLUMN `owner_id` VARCHAR(45) NOT NULL DEFAULT '' AFTER `type`,");
		sb.append("ADD COLUMN `is_visible` BIT(1) NOT NULL DEFAULT b'1' AFTER `owner_id`,");
		sb.append("ADD INDEX `idx_metadata_index_owner_id` (`owner_id` ASC),");
		sb.append("ADD INDEX `idx_metadata_index_name` (`name` ASC)");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());
		jdbcTemplate.update("UPDATE `metadata_index` SET `parent_id` = '' WHERE `parent_id` = '0'", new MapSqlParameterSource());
		jdbcTemplate.update("UPDATE `metadata_index` SET `original_index_id` = '' WHERE `original_index_id` = '0'", new MapSqlParameterSource());

		System.out.println("merging metadata_index and metadata...");
		mergeMetadataIndex();
		changeMetadataIndexIdAndParentIdAndOwnerId();

		System.out.println("updating shared_root_id...");
		updateSharedRootId();

		System.out.println("copy additional_attrs...");
		copyAdditionalAttrs();

		System.out.println("moving my_backup_data...");
		moveMyBackupData();

		System.out.println("drop unnecessary fields...");
		sb = new StringBuilder();
		sb.append("ALTER TABLE `metadata_index` DROP INDEX `idx_metadata_index_parent_id_original_index_id_id`,");
		sb.append("DROP COLUMN `files_count`,");
		sb.append("DROP COLUMN `folders_count`,");
		sb.append("DROP COLUMN `backup_no`,");
		sb.append("DROP COLUMN `metadata_id`,");
		sb.append("DROP COLUMN `in_date`,");
		sb.append("DROP COLUMN `original_index_id`");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());
	}

	private void mergeMetadataIndex() {
		jdbcTemplate
				.update("UPDATE metadata_index mi LEFT JOIN metadata m ON (m.id = mi.metadata_id) SET mi.modified_at = mi.in_date * 1000, mi.modified_by = COALESCE(m.last_edit_user, ''), mi.created_at = COALESCE(m.in_date, mi.in_date, 0) * 1000, mi.created_by = COALESCE(m.owner, ''), mi.block_id = COALESCE(m.block_id, ''), mi.`type` = COALESCE(m.`type`, 'normal')",
						new MapSqlParameterSource());
		jdbcTemplate.update("UPDATE metadata_index mi SET mi.modified_by = COALESCE(mi.name, ''), mi.created_by = COALESCE(mi.name, '') WHERE mi.parent_id = ''", new MapSqlParameterSource());
	}

	private void changeMetadataIndexIdAndParentIdAndOwnerId() {
		metadataIndexMapping = new HashMap<String, String>();

		List<String> subResult = jdbcTemplate.queryForList("SELECT `name` FROM user_info WHERE status = 'active'", new MapSqlParameterSource(), String.class);
		for (String userId : subResult) {
			try {
				String rootId = jdbcTemplate.queryForObject("SELECT id FROM metadata_index WHERE parent_id = '' AND `name` = :userId LIMIT 1", new MapSqlParameterSource().addValue("userId", userId), String.class);
				List<String> ignoreMetadataIndexIds = new ArrayList<String>();
				try {
					String myOtherFoldersId = jdbcTemplate.queryForObject("SELECT id FROM metadata_index WHERE parent_id = :rootId AND `name` = 'my other folders' LIMIT 1",
							new MapSqlParameterSource().addValue("rootId", rootId), String.class);
					ignoreMetadataIndexIds.add(myOtherFoldersId);
				} catch (EmptyResultDataAccessException e) {
				}
				subChangeMetadataIndexIdAndParentIdAndOriginalIndexId(userId, rootId, "", ignoreMetadataIndexIds);
			} catch (EmptyResultDataAccessException e) {
			}
		}
		jdbcTemplate.update("DELETE FROM metadata_index WHERE char_length(id) != 36", new MapSqlParameterSource());
		jdbcTemplate.update("UPDATE metadata_index SET original_index_id = '' WHERE original_index_id IS NOT NULL AND original_index_id != '' AND char_length(original_index_id) != 36", new MapSqlParameterSource());
	}

	private void subChangeMetadataIndexIdAndParentIdAndOriginalIndexId(String userId, String metadataIndexId, String newParentMetadataIndexId, List<String> ignoreMetadataIndexIds) {
		if (ignoreMetadataIndexIds != null && ignoreMetadataIndexIds.contains(metadataIndexId)) {
			return;
		}
		String newMetadataIndexId = generateUUID();

		jdbcTemplate.update(
				"UPDATE metadata_index SET id = :newMetadataIndexId, parent_id = :newParentMetadataIndexId, owner_id = :ownerId WHERE id = :metadataIndexId",
				new MapSqlParameterSource().addValue("newMetadataIndexId", newMetadataIndexId).addValue("newParentMetadataIndexId", newParentMetadataIndexId).addValue("metadataIndexId", metadataIndexId)
						.addValue("ownerId", userId));
		jdbcTemplate.update("UPDATE metadata_index SET original_index_id = :newMetadataIndexId WHERE original_index_id = :metadataIndexId", new MapSqlParameterSource().addValue("newMetadataIndexId", newMetadataIndexId)
				.addValue("metadataIndexId", metadataIndexId));
		metadataIndexMapping.put(metadataIndexId, newMetadataIndexId);

		List<String> subResult = jdbcTemplate.queryForList("SELECT mi.id FROM metadata_index mi WHERE mi.parent_id = :metadataIndexId", new MapSqlParameterSource("metadataIndexId", metadataIndexId), String.class);
		for (String childMetadataIndexId : subResult) {
			subChangeMetadataIndexIdAndParentIdAndOriginalIndexId(userId, childMetadataIndexId, newMetadataIndexId, ignoreMetadataIndexIds);
		}
	}

	private void updateSharedRootId() {
		List<String> subResult = jdbcTemplate.queryForList("SELECT `name` FROM user_info WHERE status = 'active'", new MapSqlParameterSource(), String.class);
		for (String userId : subResult) {
			try {
				String rootId = jdbcTemplate.queryForObject("SELECT id FROM metadata_index WHERE parent_id = '' AND `name` = :userId LIMIT 1", new MapSqlParameterSource().addValue("userId", userId), String.class);
				String mySharedFoldersId = jdbcTemplate.queryForObject("SELECT id FROM metadata_index WHERE parent_id = :rootId AND `name` = 'my shared folders' LIMIT 1",
						new MapSqlParameterSource().addValue("rootId", rootId), String.class);

				List<String> subResult2 = jdbcTemplate.queryForList("SELECT id FROM metadata_index WHERE parent_id = :mySharedFoldersId", new MapSqlParameterSource("mySharedFoldersId", mySharedFoldersId), String.class);
				for (String sharedFolderId : subResult2) {
					String sharedMetadataIndexIds = toInCause(fetchMetadataIndexIdByParentId(sharedFolderId, null, new ArrayList<String>()));
					if (sharedMetadataIndexIds.isEmpty()) {
						continue;
					}
					jdbcTemplate.update("UPDATE metadata_index SET shared_root_id = :sharedFolderId WHERE id IN (" + sharedMetadataIndexIds + ")", new MapSqlParameterSource().addValue("sharedFolderId", sharedFolderId));
				}
			} catch (EmptyResultDataAccessException e) {
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void copyAdditionalAttrs() {
		List<String> subResult = jdbcTemplate.queryForList("SELECT `name` FROM user_info WHERE status = 'active'", new MapSqlParameterSource(), String.class);
		for (String userId : subResult) {
			String myBackupDataId = null;
			try {
				String rootId = jdbcTemplate.queryForObject("SELECT id FROM metadata_index WHERE parent_id = '' AND `name` = :userId LIMIT 1", new MapSqlParameterSource().addValue("userId", userId), String.class);
				try {
					myBackupDataId = jdbcTemplate.queryForObject("SELECT id FROM metadata_index WHERE parent_id = :rootId AND `name` = 'my backup data' LIMIT 1", new MapSqlParameterSource().addValue("rootId", rootId),
							String.class);

					String backupMetadataIndexIds = toInCause(fetchMetadataIndexIdByParentId(myBackupDataId, null, new ArrayList<String>()));
					if (backupMetadataIndexIds.isEmpty() == false) {
						List<MetadataIndex> subResult2 = jdbcTemplate.query("SELECT id, folders_count, files_count, size, params FROM metadata_index WHERE id IN (" + backupMetadataIndexIds + ")", new MapSqlParameterSource(),
								metadataIndexMapper);
						for (MetadataIndex metadataIndex : subResult2) {
							int width = 0;
							int height = 0;
							try {
								Map<String, String> paramsMap = objectMapper.readValue(metadataIndex.getParams(), HashMap.class);
								width = Integer.valueOf(paramsMap.get("img_width"));
								height = Integer.valueOf(paramsMap.get("img_height"));
							} catch (Exception e) {
							}
							jdbcTemplate
									.update("INSERT INTO metadata_index_additional_attr (metadata_index_id, is_backup, folders_count, files_count, total_size, width, height) VALUES (:metadataIndexId, 1, :foldersCount, :filesCount, :size, :width, :height)",
											new MapSqlParameterSource().addValue("metadataIndexId", metadataIndex.getId()).addValue("foldersCount", metadataIndex.getFoldersCount())
													.addValue("filesCount", metadataIndex.getFilesCount()).addValue("size", metadataIndex.getSize()).addValue("width", width).addValue("height", height));
						}
					}
				} catch (EmptyResultDataAccessException e) {
				}
				String formalMetadataIndexIds = toInCause(fetchMetadataIndexIdByParentId(rootId, Arrays.asList(myBackupDataId), new ArrayList<String>()));
				if (formalMetadataIndexIds.isEmpty() == false) {
					List<MetadataIndex> subResult2 = jdbcTemplate.query("SELECT id, folders_count, files_count, size, params FROM metadata_index WHERE id IN (" + formalMetadataIndexIds + ")", new MapSqlParameterSource(),
							metadataIndexMapper);
					for (MetadataIndex metadataIndex : subResult2) {
						int width = 0;
						int height = 0;
						try {
							Map<String, String> paramsMap = objectMapper.readValue(metadataIndex.getParams(), HashMap.class);
							width = Integer.valueOf(paramsMap.get("img_width"));
							height = Integer.valueOf(paramsMap.get("img_height"));
						} catch (Exception e) {
						}
						jdbcTemplate
								.update("INSERT INTO metadata_index_additional_attr (metadata_index_id, is_backup, folders_count, files_count, total_size, width, height) VALUES (:metadataIndexId, 0, :foldersCount, :filesCount, :size, :width, :height)",
										new MapSqlParameterSource().addValue("metadataIndexId", metadataIndex.getId()).addValue("foldersCount", metadataIndex.getFoldersCount())
												.addValue("filesCount", metadataIndex.getFilesCount()).addValue("size", metadataIndex.getSize()).addValue("width", width).addValue("height", height));
					}
				}
			} catch (EmptyResultDataAccessException e) {
			}
		}
		jdbcTemplate.update("UPDATE metadata_index SET size = 0 WHERE is_folder = 1", new MapSqlParameterSource());
	}

	private void moveMyBackupData() {
		List<String> subResult = jdbcTemplate.queryForList("SELECT `name` FROM user_info WHERE status = 'active'", new MapSqlParameterSource(), String.class);
		for (String userId : subResult) {
			try {
				String rootId = jdbcTemplate.queryForObject("SELECT id FROM metadata_index WHERE parent_id = '' AND `name` = :userId LIMIT 1", new MapSqlParameterSource().addValue("userId", userId), String.class);
				String myBackupDataId = jdbcTemplate.queryForObject("SELECT id FROM metadata_index WHERE parent_id = :rootId AND `name` = 'my backup data' LIMIT 1", new MapSqlParameterSource().addValue("rootId", rootId),
						String.class);

				String backupMetadataIndexIds = toInCause(fetchMetadataIndexIdByParentId(myBackupDataId, null, new ArrayList<String>()));
				if (backupMetadataIndexIds.isEmpty() == false) {
					jdbcTemplate
							.update("INSERT INTO backup_metadata_index (id, parent_id, name, is_folder, sort_priority, size, origin_name, modified_at, modified_by, device_uid, link_path, note, params, icon_block_id, is_encrypted, created_at, created_by, block_id, `type`, owner_id, original_index_id, backup_no) (SELECT id, parent_id, name, is_folder, sort_priority, size, origin_name, modified_at, modified_by, device_uid, link_path, note, params, icon_block_id, is_encrypted, created_at, created_by, block_id, `type`, owner_id, original_index_id, backup_no FROM metadata_index WHERE id IN ("
									+ backupMetadataIndexIds + "))", new MapSqlParameterSource());
					jdbcTemplate.update("UPDATE backup_metadata_index SET `name` = :userId, origin_name = :userId, sort_priority = 0, parent_id = '', original_index_id = :rootId WHERE id = :myBackupDataId",
							new MapSqlParameterSource().addValue("userId", userId).addValue("rootId", rootId).addValue("myBackupDataId", myBackupDataId));
					jdbcTemplate.update("DELETE FROM metadata_index WHERE id IN (" + backupMetadataIndexIds + ")", new MapSqlParameterSource());
				}
			} catch (EmptyResultDataAccessException e) {
			}
		}
	}

	/*
	 * --------------------------------------------------
	 * 
	 * migrate read_history
	 * 
	 * --------------------------------------------------
	 */
	private void migrateReadHistory() {
		System.out.println("migrating read_history...");

		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `read_history` CHANGE COLUMN `metadata_index_id` `metadata_index_id` CHAR(36) NOT NULL");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());

		if (metadataIndexMapping != null) {
			List<String> subResult = jdbcTemplate.queryForList("SELECT metadata_index_id FROM read_history WHERE char_length(metadata_index_id) != 36", new MapSqlParameterSource(), String.class);
			for (String metadataIndexId : subResult) {
				String newMetadataIndexId = metadataIndexMapping.get(metadataIndexId);
				if (newMetadataIndexId == null) {
					continue;
				}
				jdbcTemplate.update("UPDATE read_history SET metadata_index_id = :newMetadataIndexId WHERE metadata_index_id = :metadataIndexId", new MapSqlParameterSource().addValue("newMetadataIndexId", newMetadataIndexId)
						.addValue("metadataIndexId", metadataIndexId));
			}
			jdbcTemplate.update("DELETE FROM read_history WHERE char_length(metadata_index_id) != 36", new MapSqlParameterSource());
		}
	}

	/*
	 * --------------------------------------------------
	 * 
	 * migrate file_processing_status
	 * 
	 * --------------------------------------------------
	 */
	private void migrateFileProcessingStatus() {
		System.out.println("migrating file_processing_status...");

		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `file_processing_status` CHANGE COLUMN `metadata_index_id` `metadata_index_id` CHAR(36) NOT NULL");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());

		if (metadataIndexMapping != null) {
			List<String> subResult = jdbcTemplate.queryForList("SELECT metadata_index_id FROM file_processing_status WHERE char_length(metadata_index_id) != 36", new MapSqlParameterSource(), String.class);
			for (String metadataIndexId : subResult) {
				String newMetadataIndexId = metadataIndexMapping.get(metadataIndexId);
				if (newMetadataIndexId == null) {
					continue;
				}
				jdbcTemplate.update("UPDATE file_processing_status SET metadata_index_id = :newMetadataIndexId WHERE metadata_index_id = :metadataIndexId",
						new MapSqlParameterSource().addValue("newMetadataIndexId", newMetadataIndexId).addValue("metadataIndexId", metadataIndexId));
			}
			jdbcTemplate.update("DELETE FROM file_processing_status WHERE char_length(metadata_index_id) != 36", new MapSqlParameterSource());
		}
	}

	/*
	 * --------------------------------------------------
	 * 
	 * migrate reader_file_metadata
	 * 
	 * --------------------------------------------------
	 */
	private void migrateReaderFileMetadata() {
		System.out.println("migrating reader_file_metadata...");

		jdbcTemplate.update("TRUNCATE `reader_file_metadata`", new MapSqlParameterSource());
		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `reader_file_metadata` DROP COLUMN `metadata_id`,");
		sb.append("ADD COLUMN `metadata_index_id` CHAR(36) NOT NULL FIRST,");
		sb.append("DROP PRIMARY KEY,");
		sb.append("ADD PRIMARY KEY (`metadata_index_id`)");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());
	}

	/*
	 * --------------------------------------------------
	 * 
	 * migrate reader_file_metadata_detail
	 * 
	 * --------------------------------------------------
	 */
	private void migrateReaderFileMetadataDetail() {
		System.out.println("migrating reader_file_metadata_detail...");

		jdbcTemplate.update("TRUNCATE `reader_file_metadata_detail`", new MapSqlParameterSource());
		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `reader_file_metadata_detail` DROP COLUMN `metadata_id`,");
		sb.append("ADD COLUMN `metadata_index_id` CHAR(36) NOT NULL FIRST,");
		sb.append("DROP PRIMARY KEY,");
		sb.append("ADD PRIMARY KEY (`metadata_index_id`, `page_no`)");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());
	}

	/*
	 * --------------------------------------------------
	 * 
	 * migrate share_link
	 * 
	 * --------------------------------------------------
	 */
	private void migrateShareLink() {
		System.out.println("migrating share_link...");

		// jdbcTemplate.update("UPDATE share_link SET expired = COALESCE(UNIX_TIMESTAMP(STR_TO_DATE(expired, '%Y-%m-%d')) * 1000, '') WHERE expired like '%-%'",
		// new MapSqlParameterSource());
		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `share_link` CHANGE COLUMN `metadata_index_id` `metadata_index_id` CHAR(36) NOT NULL,");
		sb.append("CHANGE COLUMN `last_edit_time` `last_edit_time` BIGINT(20) NOT NULL DEFAULT 0,");
		sb.append("ADD COLUMN `is_backup` BIT(1) NOT NULL AFTER `metadata_index_id`,");
		sb.append("DROP PRIMARY KEY,");
		sb.append("ADD PRIMARY KEY (`metadata_index_id`, `is_backup`)");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());

		if (metadataIndexMapping != null) {
			List<ShareLink> subResult = jdbcTemplate.query("SELECT metadata_index_id FROM share_link WHERE char_length(metadata_index_id) != 36", new MapSqlParameterSource(), shareLinkMapper);
			for (ShareLink shareLink : subResult) {
				String metadataIndexId = shareLink.getMetadataIndexId();
				String newMetadataIndexId = metadataIndexMapping.get(metadataIndexId);
				if (newMetadataIndexId == null) {
					continue;
				}
				String newExpired = null;
				if (shareLink.getExpired() != null) {
					if (shareLink.getExpired().matches("\\d{13}")) {
						newExpired = shareLink.getExpired();
					} else {
						try {
							newExpired = String.valueOf(dateFormat.parse(shareLink.getExpired()).getTime());
						} catch (Exception e) {
						}
					}
				}
				jdbcTemplate.update("UPDATE share_link SET metadata_index_id = :newMetadataIndexId, expired = :newExpired WHERE metadata_index_id = :metadataIndexId",
						new MapSqlParameterSource().addValue("newMetadataIndexId", newMetadataIndexId).addValue("metadataIndexId", metadataIndexId).addValue("newExpired", newExpired));
			}
			jdbcTemplate.update("DELETE FROM share_link WHERE char_length(metadata_index_id) != 36", new MapSqlParameterSource());
		}

		sb = new StringBuilder();
		sb.append("ALTER TABLE `share_link` CHANGE COLUMN `expired` `expired` BIGINT(20) NULL DEFAULT 0");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());
	}

	/*
	 * --------------------------------------------------
	 * 
	 * tbl_profile
	 * 
	 * --------------------------------------------------
	 */
	private void migrateTblProfile() {
		System.out.println("migrating tbl_profile...");

		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `tbl_profile` CHANGE COLUMN `metadata_index_id` `metadata_index_id` CHAR(36) NOT NULL");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());

		if (metadataIndexMapping != null) {
			List<String> subResult = jdbcTemplate.queryForList("SELECT metadata_index_id FROM tbl_profile WHERE metadata_index_id != '' AND char_length(metadata_index_id) != 36 GROUP BY metadata_index_id",
					new MapSqlParameterSource(), String.class);
			for (String metadataIndexId : subResult) {
				String newMetadataIndexId = metadataIndexMapping.get(metadataIndexId);
				if (newMetadataIndexId == null) {
					continue;
				}
				jdbcTemplate.update("UPDATE tbl_profile SET metadata_index_id = :newMetadataIndexId WHERE metadata_index_id = :metadataIndexId", new MapSqlParameterSource().addValue("newMetadataIndexId", newMetadataIndexId)
						.addValue("metadataIndexId", metadataIndexId));
			}
			jdbcTemplate.update("DELETE FROM tbl_profile WHERE metadata_index_id != '' AND char_length(metadata_index_id) != 36", new MapSqlParameterSource());
		}
	}

	/*
	 * --------------------------------------------------
	 * 
	 * text_view_setting
	 * 
	 * --------------------------------------------------
	 */
	private void migrateTextViewSetting() {
		System.out.println("migrating text_view_setting...");

		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `text_view_setting` DROP COLUMN `metadata_id`,");
		sb.append("CHANGE COLUMN `metadata_index_id` `metadata_index_id` CHAR(36) NOT NULL");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());

		if (metadataIndexMapping != null) {
			List<String> subResult = jdbcTemplate.queryForList("SELECT metadata_index_id FROM tbl_profile WHERE metadata_index_id != '' AND char_length(metadata_index_id) != 36 GROUP BY metadata_index_id",
					new MapSqlParameterSource(), String.class);
			for (String metadataIndexId : subResult) {
				String newMetadataIndexId = metadataIndexMapping.get(metadataIndexId);
				if (newMetadataIndexId == null) {
					continue;
				}
				jdbcTemplate.update("UPDATE tbl_profile SET metadata_index_id = :newMetadataIndexId WHERE metadata_index_id = :metadataIndexId", new MapSqlParameterSource().addValue("newMetadataIndexId", newMetadataIndexId)
						.addValue("metadataIndexId", metadataIndexId));
			}
			jdbcTemplate.update("DELETE FROM tbl_profile WHERE metadata_index_id != '' AND char_length(metadata_index_id) != 36", new MapSqlParameterSource());
		}
	}

	/*
	 * --------------------------------------------------
	 * 
	 * user_info
	 * 
	 * --------------------------------------------------
	 */
	private void migrateUserInfo() {
		System.out.println("migrating user_info...");

		StringBuilder sb = new StringBuilder();
		sb.append("ALTER TABLE `user_info` DROP COLUMN `cellphones`,");
		sb.append("DROP COLUMN `retry_count`,");
		sb.append("DROP COLUMN `in_date`,");
		sb.append("DROP COLUMN `status`,");
		sb.append("DROP COLUMN `display_name`,");
		sb.append("DROP COLUMN `password`,");
		sb.append("DROP PRIMARY KEY,");
		sb.append("ADD PRIMARY KEY (`name`),");
		sb.append("DROP INDEX `idx_user_info_cellphones_status`");
		jdbcTemplate.update(sb.toString(), new MapSqlParameterSource());

	}

}