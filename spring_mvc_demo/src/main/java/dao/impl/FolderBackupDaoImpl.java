package dao.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;

import dao.FolderBackupDao;

@Repository
public class FolderBackupDaoImpl implements FolderBackupDao {

	@Value("${sql.get_folder_backup_id_by_criteria}")
	private String getFolderBackupIdByCriteriaSql;

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	@Override
	public String getFolderBackupIdByCriteria(String parentId, String name, String deviceUid, String fullSourcePath) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("parentId", parentId);
		paramSource.addValue("name", name);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("fullSourcePath", fullSourcePath);
		try {
			return jdbcTemplate.queryForObject(getFolderBackupIdByCriteriaSql, paramSource, String.class);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

}
