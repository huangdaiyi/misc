package dao.impl;

import java.sql.Types;
import java.util.List;

import model.SyncRelation;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.namedparam.BeanPropertySqlParameterSource;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.jdbc.core.namedparam.SqlParameterSourceUtils;
import org.springframework.stereotype.Repository;

import dao.SyncRelationDao;

@Repository
public class SyncRelationDaoImpl implements SyncRelationDao {
	
	@Value("${sql.get_sync_relation_by_criteria}")
	private String getSyncRelationByCriteriaSql;
	@Value("${sql.add_sync_relation}")
	private String addSyncRelationSql;
	@Value("${sql.delete_sync_relation}")
	private String deleteSyncRelationSql;
	@Value("${sql.delete_sync_relation_by_id}")
	private String deleteSyncRelationByIdSql;
	@Value("${sql.query_relation_on_folder}")
	private String queryRelationsOnFolderSql;
	@Value("${sql.query_relations_on_upper_level_folders}")
	private String queryRelationsOnUpperLevelFoldersSql;
	@Value("${sql.query_relations_on_folders_include_lower_level}")
	private String queryRelationsOnFoldersIncludeLowerLevelSql;
	@Value("${sql.query_relations_on_lower_level_folders}")
	private String queryRelationsOnLowerLevelFoldersSql;
	@Value("${sql.query_relations_by_user_id}")
	private String queryRelationsByUserIdSql;
	@Value("${sql.update_sync_relation_enable}")
	private String updateSyncRelationEnableSql;
	@Value("${sql.rename_source_link_folder}")
	private String renameSourceLinkFolderSql;
	@Value("${sql.rename_destination_link_folder}")
	private String renameDestinationLinkFolderSql;
	
	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	/**
	 * attribute = null means to ignore this restriction
	 */
	@Override
	public List<SyncRelation> getSyncRelationByCriteria(SyncRelation criteria) {
		BeanPropertySqlParameterSource parameterSource = new BeanPropertySqlParameterSource(criteria);
		parameterSource.registerSqlType("action", Types.VARCHAR);
		return jdbcTemplate.query(getSyncRelationByCriteriaSql, parameterSource, SyncRelation.getMapper());
	}

	@Override
	public void addSyncRelation(SyncRelation syncRelation) {
		BeanPropertySqlParameterSource parameterSource = new BeanPropertySqlParameterSource(syncRelation);
		parameterSource.registerSqlType("action", Types.VARCHAR);
		jdbcTemplate.update(addSyncRelationSql, parameterSource);
	}

	@Override
	public void deleteSyncRelation(SyncRelation syncRelation) {
		BeanPropertySqlParameterSource parameterSource = new BeanPropertySqlParameterSource(syncRelation);
		parameterSource.registerSqlType("action", Types.VARCHAR);
		jdbcTemplate.update(deleteSyncRelationSql, parameterSource);
	}
	
	@Override
	public void deleteSyncRelation(List<SyncRelation> syncRelations) {
		SqlParameterSource[] batch = SqlParameterSourceUtils.createBatch(syncRelations.toArray());
		jdbcTemplate.batchUpdate(deleteSyncRelationByIdSql, batch);
	}
	
	@Override
	public void updateSyncRelationEnable(long id, boolean enable) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("id", id);
		parameterSource.addValue("enable", enable);
		jdbcTemplate.update(updateSyncRelationEnableSql, parameterSource);
	}
	
	@Override
	public List<SyncRelation> getRelationsOnFolderOfClient(String ownerId, String sourcePath) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", ownerId);
		parameterSource.addValue("sourcePath", sourcePath);
		parameterSource.registerSqlType("action", Types.VARCHAR);
		return jdbcTemplate.query(queryRelationsOnFolderSql, parameterSource, SyncRelation.getMapper());
	}
	
	@Override
	public List<SyncRelation> getUpperLevelFoldersOfClient(String ownerId, String sourcePath) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", ownerId);
		parameterSource.addValue("sourcePath", sourcePath);
		parameterSource.registerSqlType("action", Types.VARCHAR);
		return jdbcTemplate.query(queryRelationsOnUpperLevelFoldersSql, parameterSource, SyncRelation.getMapper());
	}

	@Override
	public List<SyncRelation> getLowerLevelFoldersOfClient(String ownerId, String sourcePath) {
		return getLowerLevelFoldersOfClient(ownerId, sourcePath, false);
	}

	@Override
	public List<SyncRelation> getLowerLevelFoldersOfClient(String ownerId, String sourcePath, boolean selfIncluded) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", ownerId);
		parameterSource.addValue("sourcePath", sourcePath);
		parameterSource.registerSqlType("action", Types.VARCHAR);
		return jdbcTemplate.query(
				selfIncluded ? queryRelationsOnFoldersIncludeLowerLevelSql : queryRelationsOnLowerLevelFoldersSql
				, parameterSource, SyncRelation.getMapper());
	}
	
	@Override
	public List<SyncRelation> getSyncRelationsByUserId(String userId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("ownerId", userId);
		parameterSource.registerSqlType("action", Types.VARCHAR);
		return jdbcTemplate.query(queryRelationsByUserIdSql, parameterSource, SyncRelation.getMapper());
	}
	
	@Override
	public void renameLinkFolder(String ownerId, String oldFolderSourcePath, String newFolderSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("ownerId", ownerId);
		paramSource.addValue("oldFolderSourcePath", oldFolderSourcePath);
		paramSource.addValue("newFolderSourcePath", newFolderSourcePath);
		
		jdbcTemplate.update(renameSourceLinkFolderSql, paramSource);
		jdbcTemplate.update(renameDestinationLinkFolderSql, paramSource);
	}

}
