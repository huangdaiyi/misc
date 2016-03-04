package dao.impl;

import java.sql.Types;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.PostConstruct;

import model.BackupMetadata;
import model.BackupMetadata4Delete;
import model.BackupMetadata4Modify;
import model.BaseMetadata;

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
import constants.HttpStatus;
import dao.BackupMetadataDao;
import dao.FolderBackupDao;
import exception.MetadataException;

@Repository
public class BackupMetadataDaoImpl implements BackupMetadataDao {

	@Value("${sql.create_file_backup_metadata}")
	private String createFileBuckupMetadataSql;
	@Value("${sql.create_folder_backup_metadata}")
	private String createFolderBuckupMetadataSql;
	@Value("${sql.get_backup_index_id}")
	private String getBackupIndexIdSql;
	@Value("${sql.get_backup_by_original_id}")
	private String getBackupByOriginalIdSql;
	@Value("${sql.get_backup}")
	private String getBackupSql;
	@Value("${sql.get_backup_parent_id}")
	private String getBackupParentIdSql;
	@Value("${sql.count_sub_backups_by_parent_id}")
	private String countSubBackupsByParentIdSql;
	@Value("${sql.get_backup_by_original_ids}")
	private String getBackupByOriginalIdsSql;
	@Value("${sql.get_sub_folder}")
	private String getSubFolderSql;
	@Value("${sql.modify_backup_no}")
	private String modifyBackupNoSql;
	@Value("${sql.delete_backup}")
	private String deleteBackupByOriginIdAndBackupNoSql;
	@Value("${sql.delete_backup_by_id}")
	private String deleteBackupByIdSql;
	@Value("${sql.delete_backup_by_ids}")
	private String deleteBackupByIdsSql;
	@Value("${sql.unbackup_file}")
	private String unbackupFileSql;
	@Value("${sql.batchunbackup_file}")
	private String batchUnbackUp;
	@Value("${sql.get_root_id}")
	private String getRootIdSql;
	@Value("${sql.get_root}")
	private String getRootSql;
	@Value("${sql.get_latest_backup}")
	private String getLatestBackupSql;
	@Value("${sql.update_backup}")
	private String updateBackupSql;
	@Value("${sql.clear_backup}")
	private String clearBackupSql;
	@Value("${sql.get_unbackup_metadatas}")
	private String getUnbackupMetadatasSql;
	@Value("${sql.get_all_unbackup_metadatas}")
	private String getAllUnbackupMetadatasSql;
	@Value("${sql.update_backup_note}")
	private String updateBackupNoteSql;
	@Value("${sql.get_backup_file_by_criteria}")
	private String getBackupFileByCriteriaSql;
	@Value("${sql.get_backup_folder_by_criteria}")
	private String getBackupFolderByCriteriaSql;
	//get_backupMetdatas_by_criteria
	@Value("${sql.get_backupMetdatas_by_criteria}")
	private String getBackupMetdatasByCriteria;
	@Value("${sql.get_backup_metadata_by_parent_id}")
	private String getBackupMetadataByParentIdSql;
	@Value("${sql.get_all_backup_metadatas_by_device_uid}")
	private String getAllBackupMetadatasByDeviceUidSql;
	@Value("${sql.exist_backup_name}")
	private String existBackupNameSql;
	@Value("${sql.rename_backup_link_folder}")
	private String renameLinkFolderSql;
	@Value("${sql.unbakcup_by_backup_count}")
	private String unbackupByBackupCountSql;
	@Value("${sql.batch_unbakcup_by_id}")
	private String batchUnbackupByIdSql;
	@Value("${sql.delete_backup_by_original_ids}")
	private String deleteBackupByOriginalIdsSql;
	@Value("${sql.unbackup_files}")
	private String unbackupFilesSql;
	@Value("${sql.update_backup_file_encryption}")
	private String updateBackupFileEncryptionSql;
	@Value("${sql.get_backup_metadatas_by_searching_name_and_note}")
	private String getBackupMetadatasBySearchingNameAndNoteSql;
	@Value("${sql.get_backup_sub_by_parent_ids_and_names}")
	private String getBackupSubByParentIdsAndNamesSql;
	@Value("${sql.update_backup_as_link_file}")
	private String updateBackupAsLinkFileSql;
	@Value("${sql.get_backup_metadatas_by_block_id}")
	private String getBackupMetadatasByBlockIdSql;
	@Value("${sql.get_unbackup_metadatas_by_parent_ids}")
	private String getUnbackupMetadatasByParentIdsSql;
	private BeanPropertyRowMapper<BackupMetadata> backupRowMapper = new BeanPropertyRowMapper<BackupMetadata>(
			BackupMetadata.class);

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;
	@Autowired
	private FolderBackupDao folderBackupDao;

	@PostConstruct
	private void init() {
		backupRowMapper.setPrimitivesDefaultedForNullValue(true);
	}

	@Override
	public BackupMetadata createFileBuckup(BackupMetadata metadata) {
		return createBackup(createFileBuckupMetadataSql, metadata);
	}

	@Override
	public List<BackupMetadata> batchFileCreateBackup(
			List<BackupMetadata> metadatas) {
		batchCreateBuckup(createFileBuckupMetadataSql, metadatas);
		return metadatas;
	}

	@Override
	public BackupMetadata createFolderBackup(BackupMetadata metadata) {
		return createBackup(createFolderBuckupMetadataSql, metadata);
	}

	@Override
	public List<BackupMetadata> batchFolderCreateBackup(
			List<BackupMetadata> metadatas) {
		batchCreateBuckup(createFolderBuckupMetadataSql, metadatas);
		return metadatas;
	}

	@Override
	public List<BackupMetadata4Modify> bathUpdateBackupNo(
			List<BackupMetadata4Modify> modifies) {
		SqlParameterSource[] params = SqlParameterSourceUtils
				.createBatch(modifies.toArray());
		jdbcTemplate.batchUpdate(modifyBackupNoSql, params);
		return modifies;
	}

	@Override
	public void deleteBackup(String originalId, int backupNo) {
		jdbcTemplate.update(
				deleteBackupByOriginIdAndBackupNoSql,
				new MapSqlParameterSource().addValue("originalIndexId",
						originalId, Types.VARCHAR).addValue("backupNo",
						backupNo));

	}

	@Override
	public void deleteBackupsByOriginalIds(List<String> originalIds) {
		if (originalIds.isEmpty()) {
			return;
		}
		jdbcTemplate.update(deleteBackupByOriginalIdsSql,
				new MapSqlParameterSource().addValue("originalIndexIds",
						originalIds));
	}

	@Override
	public void batchDeleteBackup(List<BackupMetadata4Delete> deletions) {
		SqlParameterSource[] params = SqlParameterSourceUtils
				.createBatch(deletions.toArray());
		jdbcTemplate.batchUpdate(deleteBackupByOriginIdAndBackupNoSql, params);
	}

	@Override
	public void deleteBackupById(String backupMedataIndexId) {
		jdbcTemplate.update(deleteBackupByIdSql, new MapSqlParameterSource()
				.addValue("id", backupMedataIndexId, Types.VARCHAR));
	}

	@Override
	public void deleteBackupByIds(List<String> backupMedataIndexIds) {
		if (backupMedataIndexIds.isEmpty()) {
			return;
		}
		jdbcTemplate.update(deleteBackupByIdsSql, new MapSqlParameterSource(
				"idList", backupMedataIndexIds));
	}

	@Override
	public List<BackupMetadata> getBackupByOriginalId(String originalId) {
		try {
			return jdbcTemplate.query(getBackupByOriginalIdSql,
					new MapSqlParameterSource().addValue("originalIndexId",
							originalId, Types.VARCHAR), backupRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public BackupMetadata getBackup(String id) {
		try {
			return jdbcTemplate.queryForObject(getBackupSql,
					new MapSqlParameterSource().addValue("id", id),
					backupRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public List<BackupMetadata> getBackups(List<String> originalIds) {
		if (originalIds.isEmpty()) {
			return null;
		}
		return jdbcTemplate.query(getBackupByOriginalIdsSql,
				new MapSqlParameterSource("originalIndexIds", originalIds),
				backupRowMapper);

	}

	private BackupMetadata createBackup(String createSql,
			BackupMetadata metadata) {
		jdbcTemplate.update(createSql, new BeanPropertySqlParameterSource(
				metadata));
		return metadata;
	}

	private List<BackupMetadata> batchCreateBuckup(String createSql,
			List<BackupMetadata> metadatas) {
		SqlParameterSource[] params = SqlParameterSourceUtils
				.createBatch(metadatas.toArray());
		jdbcTemplate.batchUpdate(createSql, params);
		return metadatas;
	}

	@Override
	public String cancelBackupFile(String originalIndexId) {
		jdbcTemplate.update(unbackupFileSql, new MapSqlParameterSource()
				.addValue("originalIndexId", originalIndexId, Types.VARCHAR));
		return originalIndexId;
	}

	@Override
	public void cancelBackupFile(List<String> originalIndexIds) {
		if (originalIndexIds.isEmpty()) {
			return;
		}
		jdbcTemplate.update(unbackupFilesSql, new MapSqlParameterSource()
				.addValue("originalIndexIds", originalIndexIds));
	}

	@Override
	public void batchCancelBackupFile(List<String> ids) {// ***********************************************
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("ids", ids);
		jdbcTemplate.update(batchUnbackUp, param);
	}

	@Override
	public String getRootId(String ownerId) {
		try {
			return jdbcTemplate.queryForObject(getRootIdSql,
					new MapSqlParameterSource().addValue("ownerId", ownerId,
							Types.VARCHAR), String.class);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;

	}

	@Override
	public BackupMetadata getRoot(String ownerId) {
		try {
			return jdbcTemplate.queryForObject(getRootSql,
					new MapSqlParameterSource().addValue("ownerId", ownerId,
							Types.VARCHAR), backupRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;

	}

	@Override
	public BackupMetadata getLatestBackup(String originalIndexId) {
		try {
			return jdbcTemplate.queryForObject(getLatestBackupSql,
					new MapSqlParameterSource().addValue("originalIndexId",
							originalIndexId, Types.VARCHAR), backupRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public BackupMetadata updateBackup(BackupMetadata backupMetadata) {
		jdbcTemplate.update(updateBackupSql,
				new BeanPropertySqlParameterSource(backupMetadata));
		return backupMetadata;
	}

	@Override
	public List<BackupMetadata> batchUpdateBackup(
			List<BackupMetadata> backupMetadatas) {
		SqlParameterSource[] params = SqlParameterSourceUtils
				.createBatch(backupMetadatas.toArray());
		jdbcTemplate.batchUpdate(updateBackupSql, params);
		return backupMetadatas;
	}

	@Override
	public void clearBackup(String ownerId) {
		jdbcTemplate.update(clearBackupSql, new MapSqlParameterSource()
				.addValue("ownerId", ownerId, Types.VARCHAR));
	}

	@Override
	public List<BackupMetadata> getSubFolder(String parentId) {
		return jdbcTemplate.query(getSubFolderSql,
				new MapSqlParameterSource().addValue("parentId", parentId),
				backupRowMapper);
	}

	@Override
	public long countSubBackupsByParentId(String parentId, boolean isFolder) {
		return jdbcTemplate.queryForObject(countSubBackupsByParentIdSql, new MapSqlParameterSource().addValue("parentId", parentId).addValue("folder", isFolder), Integer.class);
	}

	@Override
	public String getBackupIndexId(String ownerId, String name, String parentId) {
		try {
			return jdbcTemplate.queryForObject(
					getBackupIndexIdSql,
					new MapSqlParameterSource()
							.addValue("ownerId", ownerId, Types.VARCHAR)
							.addValue("name", name, Types.VARCHAR)
							.addValue("parentId", parentId, Types.VARCHAR),
					String.class);

		} catch (EmptyResultDataAccessException e) {
		}
		return null;

	}

	@Override
	public List<BackupMetadata> getAllUnbackupMetadatas(String ownerId) {

		return jdbcTemplate.query(getAllUnbackupMetadatasSql,
				new MapSqlParameterSource().addValue("ownerId", ownerId),
				backupRowMapper);
	}

	@Override
	public List<BackupMetadata> getUnbackupMetadatas(String ownerId,
			String parentId) {

		return jdbcTemplate.query(getUnbackupMetadatasSql,
				new MapSqlParameterSource().addValue("ownerId", ownerId)
						.addValue("parentId", parentId), backupRowMapper);
	}

	@Override
	public String getBackupParentId(String id) {
		try {
			return jdbcTemplate.queryForObject(getBackupParentIdSql,
					new MapSqlParameterSource().addValue("id", id),
					String.class);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public void updateBackupNote(String id, String note, long modifiedAt,
			String userId) {
		jdbcTemplate.update(
				updateBackupNoteSql,
				new MapSqlParameterSource().addValue("id", id, Types.VARCHAR)
						.addValue("note", note, Types.VARCHAR)
						.addValue("modifiedAt", modifiedAt, Types.BIGINT)
						.addValue("modifiedBy", userId, Types.VARCHAR));
	}

	@Override
	public BackupMetadata getBackupFolderByCriteria(String parentId,
			String name, String deviceUid, String fullSourcePath) {

		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("parentId", parentId);
		paramSource.addValue("name", name);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("fullSourcePath", fullSourcePath);
		try {
			return jdbcTemplate.queryForObject(getBackupFolderByCriteriaSql,
					paramSource, backupRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}
	
	
	@Override// becuase have unbackup metadata so  ---> is list
	public List<BackupMetadata> getBackupMetdatasByCriteria(String parentId,String name, String deviceUid, String fullSourcePath){
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("parentId", parentId);
		paramSource.addValue("name", name);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("fullSourcePath", fullSourcePath);
			return jdbcTemplate.query(getBackupMetdatasByCriteria,
					paramSource, backupRowMapper);
	}

	@Override
	public BackupMetadata getBackupFileByCriteria(String parentId, String name,
			String deviceUid, String fullSourcePath) {

		int index = -1;
		int len = name.length();
		if (StringUtils.isNullOrEmpty(name)
				|| (index = name.lastIndexOf("_")) < 1 || index > len - 1) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		String strNo = name.substring(index + 1);
		String realName = name.substring(0, index);

		index = strNo.indexOf(".");
		if (index > 0) {
			realName = realName.concat(strNo.substring(index));
			strNo = strNo.substring(0, index);
		}

		int backupNo = 0;
		try {
			backupNo = Integer.parseInt(strNo);
		} catch (RuntimeException e) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}

		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("parentId", parentId);
		paramSource.addValue("name", realName);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("fullSourcePath", fullSourcePath);
		paramSource.addValue("backupNo", backupNo);
		try {
			return jdbcTemplate.queryForObject(getBackupFileByCriteriaSql,
					paramSource, backupRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	/**
	 * path like this: userId/aaa/bbb.jpg
	 */
	@Override
	public BackupMetadata getBackupMetadataByPath(String path,
			String deviceUid, String fullSourcePath, boolean isFolder) {
		String[] pathFragmenets = path.toLowerCase().split("/");
		String parentId = "";
		int pathFragmentSize = pathFragmenets.length;
		for (int i = 0; i < pathFragmentSize; i++) {
			String pathFragmenet = pathFragmenets[i];
			if (i < pathFragmentSize - 1) {
				parentId = folderBackupDao.getFolderBackupIdByCriteria(
						parentId, pathFragmenet, "", "");
				if (parentId == null) {
					throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
				}
			} else {
				if (isFolder) {
					return getBackupFolderByCriteria(parentId, pathFragmenet,
							deviceUid, fullSourcePath);
				}
				return getBackupFileByCriteria(parentId, pathFragmenet,
						deviceUid, fullSourcePath);
			}
		}
		return null;
	}

	@Override
	public List<BackupMetadata> getBackupMetadatasByParentId(String parentId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("parentId", parentId);
		return jdbcTemplate.query(getBackupMetadataByParentIdSql,
				parameterSource, backupRowMapper);
	}

	@Override
	public List<BackupMetadata> getAllBackupMetadatasByDeviceUid(
			String deviceUid) {
		return jdbcTemplate.query(getAllBackupMetadatasByDeviceUidSql,
				new MapSqlParameterSource().addValue("deviceUid", deviceUid),
				backupRowMapper);
	}

	@Override
	public List<BackupMetadata> ExistBackupName(String metadataName,
			String backupParentId, String sourcePath) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("name", metadataName);
		// parameterSource.addValue("fileName", String.format("%s\\_%%",
		// metadataName));
		parameterSource.addValue("sourcePath", sourcePath);
		parameterSource.addValue("parentId", backupParentId);
		return jdbcTemplate.query(existBackupNameSql, parameterSource,
				backupRowMapper);
	}

	@Override
	public List<BackupMetadata> getBackupMetadatasByPath(
			String[] pathFragmenets, String deviceUid, String fullSourcePath,
			int leave, boolean isFolder) {
		int pathFragmentSize = pathFragmenets.length;
		if (leave < 0 || leave > pathFragmentSize) {
			throw new RuntimeException(
					"leave must be greater than and less than pathFragmenets's length");
		}
		BackupMetadata tempMetadata = null;
		List<BackupMetadata> results = new ArrayList<BackupMetadata>();
		String parentId = "";
		for (int i = 0; i < pathFragmentSize - 1; i++) {
			String pathFragmenet = pathFragmenets[i];
			tempMetadata = getBackupFolderByCriteria(parentId, pathFragmenet,
					"", "");
			if (null == tempMetadata) {
				throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
			}
			parentId = tempMetadata.getId();

			if (i >= leave) {
				results.add(tempMetadata);
			}

		}

		tempMetadata = isFolder ? getBackupFolderByCriteria(parentId,
				pathFragmenets[pathFragmentSize - 1], "", "")
				: getBackupFileByCriteria(parentId,
						pathFragmenets[pathFragmentSize - 1], "", "");
		results.add(tempMetadata);
		return results;
	}

	@Override
	public void renameLinkFolder(String oldFolderFullSourcePath,
			String newFolderFullSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource
				.addValue("oldFolderFullSourcePath", oldFolderFullSourcePath);
		paramSource
				.addValue("newFolderFullSourcePath", newFolderFullSourcePath);
		jdbcTemplate.update(renameLinkFolderSql, paramSource);
	}

	@Override
	public String unbackupByBackupCount(String originalIndexId, int backupCount) {
		jdbcTemplate.update(
				unbackupByBackupCountSql,
				new MapSqlParameterSource().addValue("originalIndexId",
						originalIndexId, Types.VARCHAR).addValue("backupCount",
						backupCount, Types.INTEGER));
		return originalIndexId;
	}

	@Override
	public void batchUnbackup(List<BackupMetadata> unbackupBackupMetadatas) {
		SqlParameterSource[] params = SqlParameterSourceUtils
				.createBatch(unbackupBackupMetadatas.toArray());
		jdbcTemplate.batchUpdate(batchUnbackupByIdSql, params);
	}

	@Override
	public void updateBackupFileEncryption(String id, String blockId,
			boolean encrypted, long modifiedAt, String userId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", id);
		paramSource.addValue("blockId", blockId);
		paramSource.addValue("encrypted", encrypted);
		paramSource.addValue("modifiedAt", modifiedAt);
		paramSource.addValue("modifiedBy", userId);
		try {
			jdbcTemplate.update(updateBackupFileEncryptionSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public List<? extends BaseMetadata> getBackupMetadatasBySearchingNameAndNote(
			String ownerId, String keyword) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", ownerId).addValue("keyword",
				keyword);
		return jdbcTemplate.query(getBackupMetadatasBySearchingNameAndNoteSql,
				parameterSource, backupRowMapper);
	}

	@Override
	public List<BackupMetadata> getSubBackupByParentsAndNames(String ownerId,
			List<String> parentIds, List<String> names) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("ownerId", ownerId);
		paramSource.addValue("parentIds", parentIds);
		paramSource.addValue("names", names);

		return jdbcTemplate.query(getBackupSubByParentIdsAndNamesSql,
				paramSource, backupRowMapper);
	}

	@Override
	public void updateBackupAsLinkFile(String id, String deviceUid, String fullSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", id);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("fullSourcePath", fullSourcePath);
		try {
			jdbcTemplate.update(updateBackupAsLinkFileSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}
	
	@Override
	public List<String> getAllUpperIds(String id) {
		List<String> list = new ArrayList<String>();
		while (!StringUtils.isNullOrEmpty(id)) {
			String parentId = getBackupParentId(id);
			id = parentId;
			if (!StringUtils.isNullOrEmpty(id)) {
				list.add(id);
			}
		}
		return list;
	}
	
	@Override
	public List<BackupMetadata> getBackupMetadatasByBlockId(String ownerId,
			String blockId) {
		return jdbcTemplate.query(getBackupMetadatasByBlockIdSql,
				new MapSqlParameterSource().addValue("ownerId", ownerId)
						.addValue("blockId", blockId), backupRowMapper);
	}

	@Override
	public List<BackupMetadata> getUnBackupMetdatasByParentIds(List<String> parentIds) {
		if (parentIds.isEmpty()) {
			return null;
		}
		return jdbcTemplate.query(getUnbackupMetadatasByParentIdsSql,
				new MapSqlParameterSource("parentIds", parentIds),
				backupRowMapper);
	}
}
