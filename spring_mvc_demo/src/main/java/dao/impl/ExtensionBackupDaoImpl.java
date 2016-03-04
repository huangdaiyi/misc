package dao.impl;

import java.sql.Types;
import java.util.List;

import model.DefaultBackupSetting;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.jdbc.core.namedparam.SqlParameterSourceUtils;
import org.springframework.stereotype.Repository;

import dao.ExtensionBackupDao;

@Repository
public class ExtensionBackupDaoImpl implements ExtensionBackupDao {

	private static final Logger logger = LogManager.getLogger(ExtensionBackupDaoImpl.class);
	
	@Value("${sql.get_extension_backup}")
	private String getExtensionBackupSql;
	@Value("${sql.add_extension_backup}")
	private String addExtensionBackupSql;
	@Value("${sql.delete_extension_backup}")
	private String deleteExtensionBackupSql;
	@Value("${sql.update_extension_backup}")
	private String updateUserExtensionBackupSql;
	
	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;
	// bean row mapper
	private BeanPropertyRowMapper<DefaultBackupSetting> defaultBackupSettingBeanRowMapper = 
			    new BeanPropertyRowMapper<DefaultBackupSetting>(
			    DefaultBackupSetting.class);
	
	@Override
	public  List<DefaultBackupSetting> getExtensionBackup(String userId) {
		return jdbcTemplate.query(
				getExtensionBackupSql,
				new MapSqlParameterSource().addValue("userId", userId,Types.VARCHAR),
				defaultBackupSettingBeanRowMapper);
	}
	
	@Override
	public void addExtensionBackup(String userId, String extension) {
		try{
	        jdbcTemplate.update(addExtensionBackupSql,
				new MapSqlParameterSource()
						.addValue("userId", userId)
						.addValue("extension", extension));
		}catch(DuplicateKeyException de){
			logger.error("addExtensionBackup error on DuplicateKeyException");
		}
	}
	
	@Override
	public void deleteExtensionBackup(String userId,List<Integer> idList) {
		if (idList.isEmpty()){
			return;
		}
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("idList", idList).addValue("userId", userId);
		jdbcTemplate.update(deleteExtensionBackupSql, paramSource);
	}
	
	@Override
	public void batchupdateExtensionBackup(List<DefaultBackupSetting> updateUserExtensionbackups) {
		SqlParameterSource[] params = SqlParameterSourceUtils
				.createBatch(updateUserExtensionbackups.toArray());
		jdbcTemplate.batchUpdate(updateUserExtensionBackupSql, params);
	}

}
