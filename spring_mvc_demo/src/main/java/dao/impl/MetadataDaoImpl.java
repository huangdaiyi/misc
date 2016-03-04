package dao.impl;

import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;

import model.BaseMetadata;
import model.ItemStyle;
import model.Metadata;
import model.SharedRootMap;

import org.apache.commons.io.FilenameUtils;
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
import constants.MetadataType;
import dao.FolderDao;
import dao.MetadataDao;
import exception.MetadataException;

@Repository
public class MetadataDaoImpl implements MetadataDao {

	@Value("${sql.create_metadata}")
	private String createMetadataSql;
	@Value("${sql.get_metadata_by_criteria}")
	private String getMetadataByCriteriaSql;
	@Value("${sql.get_metadata_by_parent_id}")
	private String getMetadataByParentIdSql;
	@Value("${sql.get_metadata_by_parent_id_and_name}")
	private String getMetadataByParentIdAndNameSql;
	@Value("${sql.get_metadata_by_type}")
	private String getMetadataByTypeSql;
	@Value("${sql.get_folders_by_parent_id}")
	private String getFoldersByParentIdSql;
	@Value("${sql.get_all_files_in_link_folder}")
	private String getAllFilesInLinkFolderSql;
	@Value("${sql.get_all_files_by_device_uid}")
	private String getAllFilesByDeviceUidSql;
	@Value("${sql.get_metadata}")
	private String getMetadataSql;
	@Value("${sql.get_root_metadata_id}")
	private String getRootMetadataIdSql;
	@Value("${sql.get_user_allmetadatanode}")
	private String getAllmetadataNodeByOwnerid;
	@Value("${sql.get_owner_metadta_by_extensions}")
	private String getOwnerMetadatasByExtensionsSql;
	@Value("${sql.delete_metadata}")
	private String deleteMetadataSql;
	@Value("${sql.batch_delete_metadata}")
	private String batchDeleteMetadataString;
	@Value("${sql.update_note}")
	private String updateNoteSql;
	@Value("${sql.update_new_name}")
	private String updateNewNameSql;
	@Value("${sql.update_new_name_to_reflink}")
	private String updateNewNameToReflinkSql;
	@Value("${sql.update_backup_count}")
	private String updateBackupCountSql;
	@Value("${sql.clean_metadata_icon}")
	private String cleanMetadataIconSql;
	@Value("${sql.update_metadata_icon}")
	private String updateMetadataIconSql;
	@Value("${sql.update_metadata}")
	private String updateMetadataSql;
	@Value("${sql.update_metadata_parent_id}")
	private String updateMetadataParentIdSql;
	@Value("${sql.update_image_width_height}")
	private String updateImageWidthHeightSql;
	@Value("${sql.update_image_size}")
	private String updateImageSizeSql;
	@Value("${sql.update_image_block_id}")
	private String updateImageBlockIdSql;
	@Value("${sql.update_image_modified_at}")
	private String updateImageModifiedAtSql;
	@Value("${sql.update_shared_root_id}")
	private String updateSharedRootIdSql;
	@Value("${sql.update_sub_shared_root_id}")
	private String updateSubSharedRootIdSql;
	@Value("${sql.get_max_sort_priority}")
	private String getMaxSortPrioritySql;
	@Value("${sql.get_metadata_count}")
	private String getMetadataCountSql;
	@Value("${sql.batch_get_metadata}")
	private String batchGetMetadataSql;
	@Value("${sql.update_metadata_type}")
	private String updateMetadataTypeSql;
	@Value("${sql.update_sync_root_id}")
	private String updateSyncRootIdSql;
	@Value("${sql.update_to_link_file}")
	private String updateToLinkFileSql;
	@Value("${sql.rename_link_folder}")
	private String renameLinkFolderSql;
	@Value("${sql.get_metadata_by_shared_root}")
	private String searchSharedRootSql;
	@Value("${sql.get_metadata_sub_by_parent_id}")
	private String batchGetSubSql;
	@Value("${sql.get_owner_metadatas_by_block_id}")
	private String getOwnerMetadatasByBlockIdSql;
	@Value("${sql.get_metadatas_by_searching_name_and_note}")
	private String getMetadatasBySearchingNameAndNoteSql;
	@Value("${sql.update_file_encryption}")
	private String updateFileEncryptionSql;
	@Value("${sql.transform_sync_to_normal}")
	private String transformSyncToNormalSql;
	@Value("${sql.transform_sync_to_normal_reserve_sync_root_id}")
	private String transformSyncToNormalReserveSyncRootIdSql;
	@Value("${sql.get_folder_ids_by_parent_id}")
	private String getFolderIdsByParentId;
	@Value("${sql.get_unread_sub_file_count}")
	private String getUnreadSubFileCountSql;
	@Value("${sql.get_metadata_by_ref_id}")
	private String getMetadataByRefIdSql;
	@Value("${sql.get_metadata_by_ref_id_only}")
	private String getMetadataByRefIdOnlySql;
	@Value("${sql.get_refmetadata_by_shared_id}")
	private String getRefMetadataByShardIdSql;
	@Value("${sql.update_item_style}")
	private String updateItemStyleSql;
	@Value("${sql.update_sub_items_style}")
	private String updateSubItemsStyleSql;
	@Value("${sql.update_global_item_style}")
	private String updateGlobalItemStyleSql;
	@Value("${sql.get_global_item_style}")
	private String getGlobalItemStyleSql;
	@Value("${sql.count_sub_metadatas_by_parent_id}")
	private String countSubMetadatasByParentIdSql;

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;
	@Autowired
	private FolderDao folderDao;

	// bean row mapper
	private BeanPropertyRowMapper<Metadata> metadataBeanRowMapper = new BeanPropertyRowMapper<Metadata>(
			Metadata.class);
	private BeanPropertyRowMapper<BaseMetadata> baseMetadataBeanRowMapper = new BeanPropertyRowMapper<BaseMetadata>(
			BaseMetadata.class);
	private BeanPropertyRowMapper<ItemStyle> itemStyleBeanRowMapper = new BeanPropertyRowMapper<ItemStyle>(
			ItemStyle.class);

	@PostConstruct
	private void init() {
		metadataBeanRowMapper.setPrimitivesDefaultedForNullValue(true);
		baseMetadataBeanRowMapper.setPrimitivesDefaultedForNullValue(true);
	}

	@Override
	public Metadata createMetadata(Metadata metadata) {
		jdbcTemplate.update(createMetadataSql,
				new BeanPropertySqlParameterSource(metadata));
		return metadata;
	}

	@Override
	public List<Metadata> getMetadatasByParentId(String parentId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("parentId", parentId);
		return jdbcTemplate.query(getMetadataByParentIdSql, parameterSource,
				metadataBeanRowMapper);
	}

	@Override
	public List<Metadata> getFoldersByParentId(String parentId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("parentId", parentId);
		return jdbcTemplate.query(getFoldersByParentIdSql, parameterSource,
				metadataBeanRowMapper);
	}

	@Override
	public List<Metadata> getAllFilesInLinkFolder(String fullSourcePath,
			boolean includeConmmonFoders) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("fullSourcePath", fullSourcePath);
		parameterSource.addValue("includeConmmonFoders", includeConmmonFoders);
		return jdbcTemplate.query(getAllFilesInLinkFolderSql, parameterSource,
				metadataBeanRowMapper);
	}

	@Override
	public List<Metadata> getAllFilesByDeviceUid(String deviceUid) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("deviceUid", deviceUid);
		return jdbcTemplate.query(getAllFilesByDeviceUidSql, parameterSource,
				metadataBeanRowMapper);
	}

	@Override
	public List<Metadata> batchCreateMetadata(List<Metadata> metadatas) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(metadatas.toArray());
		jdbcTemplate.batchUpdate(createMetadataSql, batch);
		return metadatas;
	}

	@Override
	public Metadata getMetadata(String id) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("id", id);
		try {
			return jdbcTemplate.queryForObject(getMetadataSql, parameterSource,
					metadataBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public Metadata getMetadataByCriteria(String parentId, String name,
			String deviceUid, String fullSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("parentId", parentId);
		paramSource.addValue("name", name);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("fullSourcePath", fullSourcePath);
		try {
			return jdbcTemplate.queryForObject(getMetadataByCriteriaSql,
					paramSource, metadataBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	/**
	 * path like this: userId/aaa/bbb.jpg
	 */
	@SuppressWarnings("deprecation")
	@Override
	public Metadata getMetadataByPath(String path, String deviceUid,
			String fullSourcePath) {
		String[] pathFragmenets = path.toLowerCase().split("/");
		String parentId = "";
		StringBuilder originalPath = new StringBuilder();
		int pathFragmentSize = pathFragmenets.length;
		Metadata parentMetadata = null;
		Metadata metadata = null;
		for (int i = 0; i < pathFragmentSize; i++) {
			String pathFragmenet = pathFragmenets[i];
			if (i < pathFragmentSize - 1) {
				parentMetadata = getMetadataByCriteria(parentId, pathFragmenet,
						"", "");

				if (parentMetadata != null
						&& parentMetadata.getType().equals(MetadataType.REFLINK.toString())) {
					parentMetadata = getMetadata(parentMetadata.getRefId());
				}
				if (parentMetadata == null) {
					throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
				}
				parentId = parentMetadata.getId();
				if (i == 1) {
					originalPath.append(parentMetadata.getOriginName());
				} else if (i > 1) {
					originalPath.append("/").append(
							parentMetadata.getOriginName());
				}
			} else {
				metadata = getMetadataByCriteria(parentId, pathFragmenet,
						deviceUid, fullSourcePath);
				if (metadata != null) {
					if (metadata.getType().equals(MetadataType.REFLINK.toString())) {
						metadata = getMetadata(metadata.getRefId());
					}
					if (i == 1) {
						originalPath.append(metadata.getOriginName());
					} else if (i > 1) {
						originalPath.append("/").append(metadata.getOriginName());
					}
					// FIXME: don't use setter under this function
					metadata.setPath(originalPath.toString());
				}				
			}
		}
		return metadata;
	}

	@Override
	public List<? extends BaseMetadata> getMetadatasByPath(
			String[] pathFragmenets, String deviceUid, String fullSourcePath,
			int leave) {
		int pathFragmentSize = pathFragmenets.length;
		if (leave < 0 || leave > pathFragmentSize) {
			throw new RuntimeException(
					"leave must be greater than and less than pathFragmenets's length");
		}
		Metadata tempMetadata = null;
		List<Metadata> results = new ArrayList<Metadata>();
		String parentId = "";
		for (int i = 0; i < pathFragmentSize; i++) {
			String pathFragmenet = pathFragmenets[i];
			tempMetadata = getMetadataByCriteria(parentId, pathFragmenet, "",
					"");
			if (null == tempMetadata) {
				throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
			}
			parentId = tempMetadata.getId();

			if (i >= leave) {
				results.add(tempMetadata);
			}

		}
		return results;
	}

	@Override
	public String getRootMetadataId(String ownerId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", ownerId);
		try {
			return jdbcTemplate.queryForObject(getRootMetadataIdSql,
					parameterSource, String.class);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public void deleteMetadata(String id) {
		try {
			jdbcTemplate.update(deleteMetadataSql, new MapSqlParameterSource()
					.addValue("id", id, Types.VARCHAR));
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public void updateNote(String id, String note, long modifiedAt,
			String userId) {
		jdbcTemplate.update(
				updateNoteSql,
				new MapSqlParameterSource().addValue("id", id, Types.VARCHAR)
						.addValue("note", note, Types.VARCHAR)
						.addValue("modifiedAt", modifiedAt, Types.BIGINT)
						.addValue("modifiedBy", userId, Types.VARCHAR));
	}

	@Override
	public void updateNewName(String id, String newName, String newSourcePath) {
		jdbcTemplate.update(
				updateNewNameSql,
				new MapSqlParameterSource().addValue("id", id)
						.addValue("newName", newName.toLowerCase())
						.addValue("originNewName", newName)
						.addValue("newSourcePath", newSourcePath));
	}
	
	@Override
	public void updateNewNameToReflink(String id, String newName, String newSourcePath) {
		jdbcTemplate.update(
				updateNewNameToReflinkSql,
				new MapSqlParameterSource().addValue("refId", id)
						.addValue("newName", newName.toLowerCase())
						.addValue("originNewName", newName)
						.addValue("newSourcePath", newSourcePath));
	}

	@Override
	public void updateBackupCount(String id, Integer backupCount) {
		jdbcTemplate.update(updateBackupCountSql,
				new MapSqlParameterSource().addValue("id", id, Types.VARCHAR)
						.addValue("backupCount", backupCount, Types.INTEGER));
	}

	@Override
	public List<Metadata> getParents(String id) {
		List<Metadata> parents = new ArrayList<Metadata>();
		Metadata metadata = getMetadata(id);
		if (metadata == null) {
			return parents;
		}
		parents.add(metadata);

		while (metadata.getParentId() != null
				&& metadata.getParentId().isEmpty() == false) {
			metadata = getMetadata(metadata.getParentId());
			if (metadata != null) {
				parents.add(metadata);
			}
		}
		return parents;
	}

	@Override
	public String getFullPath(String id) {
		// List<Metadata> parents = new ArrayList<Metadata>();
		Metadata metadata = getMetadata(id);
		if (metadata == null) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}
		StringBuilder pathBuilder = new StringBuilder();
		pathBuilder.append(metadata.getOriginName());
		while (!StringUtils.isNullOrEmpty(metadata.getParentId())) {
			metadata = getMetadata(metadata.getParentId());
			if (metadata != null) {
				pathBuilder.insert(0, "/");
				pathBuilder.insert(0, metadata.getOriginName());
			}
		}
		return pathBuilder.toString();
	}

	@Override
	public Metadata updateFileMetadata(Metadata metadata) {
		jdbcTemplate.update(updateMetadataSql,
				new BeanPropertySqlParameterSource(metadata));
		return metadata;
	}

	@Override
	public List<BaseMetadata> getUserAllNodeByOwnerId(String ownerId) {
		List<BaseMetadata> lst = new ArrayList<BaseMetadata>();
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("owner_id", ownerId);
		List<Map<String, Object>> rst = jdbcTemplate.queryForList(
				getAllmetadataNodeByOwnerid, param);
		for (Map<String, Object> item : rst) {
			BaseMetadata entity = new BaseMetadata();
			entity.setId(item.get("id").toString());
			entity.setParentId(item.get("parent_id").toString());
			entity.setType(item.get("type").toString());
			lst.add(entity);
		}
		return lst;
	}

	@Override
	public List<Metadata> getOwnerMetadatasByExtensions(String ownerId,
			List<String> extensions) {
		if (extensions.isEmpty()) {
			return null;
		}
		return jdbcTemplate.query(getOwnerMetadatasByExtensionsSql,
				new MapSqlParameterSource().addValue("ownerId", ownerId)
						.addValue("extensions", extensions),
				metadataBeanRowMapper);
	}

	@Override
	public void batchDeleteMetadata(List<String> idList) {
		if (idList.isEmpty()) {
			return;
		}
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("id", idList);
		jdbcTemplate.update(batchDeleteMetadataString, param);
	}

	@Override
	public void batchCleanMetadataIcon(List<Metadata> metadatas) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(metadatas.toArray());
		jdbcTemplate.batchUpdate(cleanMetadataIconSql, batch);
	}

	@Override
	public void updateMetadataIcon(String id, String iconBlockId,
			String iconText, String iconTextColor, String iconTextStyle) {
		jdbcTemplate.update(
				updateMetadataIconSql,
				new MapSqlParameterSource().addValue("id", id)
						.addValue("iconBlockId", iconBlockId)
						.addValue("iconText", iconText)
						.addValue("iconTextColor", iconTextColor)
						.addValue("iconTextStyle", iconTextStyle));
	}

	@Override
	public void batchUpdateMetadataParentId(List<Metadata> metadatas) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(metadatas.toArray());
		jdbcTemplate.batchUpdate(updateMetadataParentIdSql, batch);
	}

	@Override
	public void updateImageWidthHeight(String id, int width, int height) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", id);
		paramSource.addValue("width", width);
		paramSource.addValue("height", height);
		try {
			jdbcTemplate.update(updateImageWidthHeightSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public void updateImageSize(String id, long size) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", id);
		paramSource.addValue("size", size);
		try {
			jdbcTemplate.update(updateImageSizeSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public void updateImageBlockId(String id, String blockId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", id);
		paramSource.addValue("blockId", blockId);
		try {
			jdbcTemplate.update(updateImageBlockIdSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public void updateImageModifiedAt(String id, long modifiedAt) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", id);
		paramSource.addValue("modifiedAt", modifiedAt);
		try {
			jdbcTemplate.update(updateImageModifiedAtSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public void updateSharedRootId(String metadataIndexId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", metadataIndexId);
		try {
			jdbcTemplate.update(updateSharedRootIdSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}
	
	@Override
	public void updateSubSharedRootId(List<SharedRootMap> sharedMaps) {
		SqlParameterSource[] batch = SqlParameterSourceUtils.createBatch(sharedMaps.toArray());
		jdbcTemplate.batchUpdate(updateSubSharedRootIdSql, batch);
	}

	@Override
	public void batchUpdateMetadata(List<Metadata> metadatas) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(metadatas.toArray());
		jdbcTemplate.batchUpdate(updateMetadataSql, batch);

	}

	@Override
	public int getMetadataCount() {
		return jdbcTemplate.queryForObject(getMetadataCountSql, new MapSqlParameterSource(), Integer.class);
	}

	@Override
	public long countSubMetadatasByParentId(String parentId, boolean isFolder) {
		return jdbcTemplate.queryForObject(countSubMetadatasByParentIdSql, new MapSqlParameterSource().addValue("parentId", parentId).addValue("folder", isFolder), Integer.class);
	}

	@Override
	public List<Metadata> batchGetMetadata(int startIndex, int endIndex) {
		MapSqlParameterSource map = new MapSqlParameterSource();
		map.addValue("offset", startIndex);
		map.addValue("rowCount", endIndex - startIndex);
		List<Metadata> result = jdbcTemplate.query(batchGetMetadataSql, map,
				metadataBeanRowMapper);
		return result;
	}

	@Override
	public List<Metadata> batchGetMetadata(String owner_id,
			List<String> parentId, List<String> name) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		String sql = batchGetSubSql;
		param.addValue("owner_id", owner_id);
		param.addValue("parentId", parentId);
		if (!StringUtils.isNullOrEmpty(name)) {
			param.addValue("name", name);
			sql = sql.concat(" AND name IN (:name) ");
		}
		List<Metadata> result = jdbcTemplate.query(sql, param,
				metadataBeanRowMapper);
		return result;

	}

	@Override
	public int getMaxSortPriority(String parentId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("parentId", parentId);
		try {
			return jdbcTemplate.queryForObject(getMaxSortPrioritySql,
					parameterSource, Integer.class);
		} catch (EmptyResultDataAccessException e) {
		}
		return 0;
	}

	@Override
	public void updateMetadataType(String metadataIndexId,
			MetadataType metadataType) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", metadataIndexId);
		paramSource.addValue("type", metadataType.toString());
		try {
			jdbcTemplate.update(updateMetadataTypeSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}

	}

	@Override
	public List<Metadata> getMetadatasByType(String ownerId, MetadataType type) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", ownerId);
		parameterSource.addValue("type", type.toString());
		return jdbcTemplate.query(getMetadataByTypeSql, parameterSource,
				metadataBeanRowMapper);
	}

	@Override
	public void updateSyncRootId(String metadataIndexId, String syncRootId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", metadataIndexId);
		paramSource.addValue("syncRootId", syncRootId);
		try {
			jdbcTemplate.update(updateSyncRootIdSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	/**
	 * path like this: my sync app backup data/appName/lastAppFolderName/bbb.jpg
	 */
	@Override
	public Metadata getSyncAppMetadataByPath(String path, String deviceUid,
			String fullSourcePath) {
		String[] pathFragmenets = path.toLowerCase().split("/");
		String parentId = "";
		int pathFragmentSize = pathFragmenets.length;
		int fragmentIndex = 0;
		for (; fragmentIndex < pathFragmentSize - 1; fragmentIndex++) {
			if (fragmentIndex == 0 || fragmentIndex == 1)
				parentId = folderDao.getFolderIdByCriteria(parentId,
						pathFragmenets[fragmentIndex], "", "");
			if (fragmentIndex == 2)
				parentId = folderDao.getFolderIdByCriteria(parentId,
						pathFragmenets[fragmentIndex], deviceUid, "");
			if (fragmentIndex == 3)
				parentId = folderDao.getFolderIdByCriteria(parentId,
						pathFragmenets[fragmentIndex], deviceUid,
						FilenameUtils.getPathNoEndSeparator(fullSourcePath));
		}
		if (fragmentIndex == 2)
			return getMetadataByCriteria(parentId,
					pathFragmenets[fragmentIndex], deviceUid, "");
		return getMetadataByCriteria(parentId, pathFragmenets[fragmentIndex],
				deviceUid, fullSourcePath);
	}

	@Override
	public List<Metadata> getMetadataByRelativePath(
			String startWithMetadataIndexId, String relativePath) {
		return getMetadataByRelativePath(startWithMetadataIndexId,
				Arrays.asList(relativePath.split("/")));
	}

	@Override
	public List<Metadata> getMetadataByRelativePath(
			String startWithMetadataIndexId, List<String> splittedRelativePath) {
		List<Metadata> result = new ArrayList<Metadata>();
		result.add(getMetadata(startWithMetadataIndexId));

		for (int i = 0; i < splittedRelativePath.size() && result.size() > 0; i++) {
			String name = splittedRelativePath.get(i);
			List<Metadata> temp = new ArrayList<Metadata>();
			for (Metadata metadata : result) {
				try {
					MapSqlParameterSource parameterSource = new MapSqlParameterSource();
					parameterSource.addValue("parentId", metadata.getId());
					parameterSource.addValue("name", name);
					temp.addAll(jdbcTemplate.query(
							getMetadataByParentIdAndNameSql, parameterSource,
							metadataBeanRowMapper));
				} catch (EmptyResultDataAccessException e) {

				}
			}
			result = temp;
		}

		return result;
	}

	@Override
	public void updateLinkFile(String metadataIndexId, String deviceUid,
			String fullSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", metadataIndexId);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("fullSourcePath", fullSourcePath);
		jdbcTemplate.update(updateToLinkFileSql, paramSource);
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
	public void batchResetBackupCount(List<Metadata> resetBackupCountMetadatas) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(resetBackupCountMetadatas.toArray());
		jdbcTemplate.batchUpdate(updateBackupCountSql, batch);
	}

	@Override
	public List<Metadata> getMetadataBySharedRootId(String sharedRootId,
			String searchKey) {
		// searchSharedRootSql
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("sharedRootId", sharedRootId);
		parameterSource.addValue("searchKey", searchKey);
		return jdbcTemplate.query(searchSharedRootSql, parameterSource,
				metadataBeanRowMapper);
	}

	@Override
	public List<BaseMetadata> getOwnerMetadatasByBlockId(String ownerId,
			String blockId) {
		return jdbcTemplate.query(getOwnerMetadatasByBlockIdSql,
				new MapSqlParameterSource().addValue("ownerId", ownerId)
						.addValue("blockId", blockId), baseMetadataBeanRowMapper);
	}

	@Override
	public List<BaseMetadata> getMetadatasBySearchingNameAndNote(
			String ownerId, String keyword) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", ownerId).addValue("keyword",
				keyword);
		return jdbcTemplate.query(getMetadatasBySearchingNameAndNoteSql,
				parameterSource, baseMetadataBeanRowMapper);
	}

	@Override
	public void updateFileEncryption(String id, String blockId,
			boolean encrypted, long modifiedAt, String userId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", id);
		paramSource.addValue("blockId", blockId);
		paramSource.addValue("encrypted", encrypted);
		paramSource.addValue("modifiedAt", modifiedAt);
		paramSource.addValue("modifiedBy", userId);
		try {
			jdbcTemplate.update(updateFileEncryptionSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public void transformSyncToNormal(String id, boolean reserveSyncRootId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("id", id);
		try {
			if(reserveSyncRootId) {
				jdbcTemplate.update(transformSyncToNormalReserveSyncRootIdSql, paramSource);
			}
			else {
				jdbcTemplate.update(transformSyncToNormalSql, paramSource);
			}
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public Metadata getMetadataByRefId(String userId, String refId) {
		if (StringUtils.isNullOrEmpty(refId)) {
			return null;
		}
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("userId", userId);
		parameterSource.addValue("refId", refId);
		try {
			return jdbcTemplate.queryForObject(getMetadataByRefIdSql, parameterSource,
					metadataBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public List<Metadata> getRefMetadataBySharedId(List<String> sharesIds) {
		if (sharesIds == null || sharesIds.isEmpty()) {
			return new ArrayList<Metadata>();
		}
		MapSqlParameterSource param = new MapSqlParameterSource();
		String sql = getRefMetadataByShardIdSql;
		param.addValue("refId", sharesIds);
		List<Metadata> result = jdbcTemplate.query(sql, param,
				metadataBeanRowMapper);
		return result;
	}
	
	@Override
	public List<Metadata> getMetadataByRefIdOnly(String refId) {
		if (StringUtils.isNullOrEmpty(refId)) {
			return new ArrayList<Metadata>();
		}
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("refId", refId);
		try {
			return jdbcTemplate.query(getMetadataByRefIdOnlySql, parameterSource,
					metadataBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}
	
	@Override
	public List<String> getFolderIdsByParentId(String parentId, boolean excludeNoAnyFileFolder) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("parentId", parentId);
		parameterSource.addValue("excludeNoAnyFileFolder", excludeNoAnyFileFolder);
		return jdbcTemplate.queryForList(getFolderIdsByParentId, parameterSource, String.class);
	}
	
	@Override
	public int getUnreadSubFileCount(String id, String userId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("id", id);
		parameterSource.addValue("userId", userId);
		return jdbcTemplate.queryForObject(getUnreadSubFileCountSql, parameterSource, Integer.class);
	}

	@Override
	public void updateItemStyle(String id, String itemTextColor, String itemBgColor,
			Boolean itemTextBold, Boolean itemTextItalic,
			Boolean itemTextUnderline) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("id", id);
		parameterSource.addValue("itemTextColor", itemTextColor);
		parameterSource.addValue("itemBgColor", itemBgColor);
		parameterSource.addValue("itemTextBold", itemTextBold);
		parameterSource.addValue("itemTextItalic", itemTextItalic);
		parameterSource.addValue("itemTextUnderline", itemTextUnderline);
		jdbcTemplate.update(updateItemStyleSql, parameterSource);
	}
	
	@Override
	public void updateSubItemsStyle(String id, String itemTextColor, String itemBgColor,
			Boolean itemTextBold, Boolean itemTextItalic,
			Boolean itemTextUnderline) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("parentId", id);
		parameterSource.addValue("itemTextColor", itemTextColor);
		parameterSource.addValue("itemBgColor", itemBgColor);
		parameterSource.addValue("itemTextBold", itemTextBold);
		parameterSource.addValue("itemTextItalic", itemTextItalic);
		parameterSource.addValue("itemTextUnderline", itemTextUnderline);
		jdbcTemplate.update(updateSubItemsStyleSql, parameterSource);
	}

	@Override
	public void updateGlobalItemStyle(String ownerId, String itemTextColor,
			String itemBgColor, Boolean itemTextBold, Boolean itemTextItalic,
			Boolean itemTextUnderline) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", ownerId);
		parameterSource.addValue("itemTextColor", itemTextColor);
		parameterSource.addValue("itemBgColor", itemBgColor);
		parameterSource.addValue("itemTextBold", itemTextBold);
		parameterSource.addValue("itemTextItalic", itemTextItalic);
		parameterSource.addValue("itemTextUnderline", itemTextUnderline);
		jdbcTemplate.update(updateGlobalItemStyleSql, parameterSource);
	}

	@Override
	public ItemStyle getGlobalItemStyle(String ownerId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", ownerId);
		return jdbcTemplate.queryForObject(getGlobalItemStyleSql, parameterSource, itemStyleBeanRowMapper);
	}
	
}
