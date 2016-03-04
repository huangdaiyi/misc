package dao.impl;


import java.util.List;

import model.GlobalSyncApp;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.jdbc.core.namedparam.SqlParameterSourceUtils;
import org.springframework.stereotype.Repository;

import dao.SyncAppDao;

@Repository
public class SyncAppDaoImpl implements SyncAppDao {

	@Value("${sql.add_global_app}")
	private String addGlobalAppSql;
	@Value("${sql.delete_global_app}")
	private String deleteGlobalAppSql;
	@Value("${sql.get_global_app}")
	private String getGlobalAppSql;
	@Value("${sql.get_global_app_by_app_name}")
	private String getGlobalAppByAppNameSql;
	@Value("${sql.add_user_defined_app}")
	private String addUserDefinedAppSql;
	@Value("${sql.delete_user_defined_app}")
	private String deleteUserDefinedAppSql;
	@Value("${sql.update_user_defined_app}")
	private String updateUserDefinedAppSql;
	@Value("${sql.get_user_defined_app_by_user}")
	private String getUserDefinedAppByUserSql;
	@Value("${sql.get_user_defined_app_by_key}")
	private String getUserDefinedAppByKeySql;
	@Value("${sql.get_user_defined_app_by_intersection}")
	private String getUserDefinedAppByIntersectionSql;

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	// bean row mapper
	private BeanPropertyRowMapper<GlobalSyncApp> appBeanRowMapper = new BeanPropertyRowMapper<GlobalSyncApp>(
			GlobalSyncApp.class);
	
	@Override
	public void addGlobalApp(List<GlobalSyncApp> globalApps) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(globalApps.toArray());
		jdbcTemplate.batchUpdate(addGlobalAppSql, batch);
	}
	
	@Override
	public void deleteGlobalApp(String appName) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("appName", appName);
		jdbcTemplate.update(deleteGlobalAppSql, paramSource);
	}

	@Override
	public List<GlobalSyncApp> getGlobalAppByAppName(String appName) {
		return jdbcTemplate.query(getGlobalAppByAppNameSql,
				new MapSqlParameterSource().addValue("appName", appName),
				appBeanRowMapper);
	}

	@Override
	public List<GlobalSyncApp> getGlobalApp() {
		return jdbcTemplate.query(getGlobalAppSql,appBeanRowMapper);
	}
	
	@Override
	public void addUserDefinedApp(List<GlobalSyncApp> globalApps) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(globalApps.toArray());
		jdbcTemplate.batchUpdate(addUserDefinedAppSql, batch);
	}
	
	@Override
	public void deleteUserDefinedApp(String userId,String deviceUniqueId,String appName) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("user", userId)
		    .addValue("deviceUniqueId", deviceUniqueId).addValue("appName", appName);
		jdbcTemplate.update(deleteUserDefinedAppSql, paramSource);
	}

	@Override
	public void updateUserDefinedApp(List<GlobalSyncApp> globalApps) {
		SqlParameterSource[] batch = SqlParameterSourceUtils
				.createBatch(globalApps.toArray());
		jdbcTemplate.batchUpdate(updateUserDefinedAppSql, batch);
	}
	
	@Override
	public List<GlobalSyncApp> getUserDefinedAppByUser(String userId) {
		return jdbcTemplate.query(getUserDefinedAppByUserSql,
				new MapSqlParameterSource().addValue("user", userId),
				appBeanRowMapper);
	}

	@Override
	public GlobalSyncApp getUserDefinedAppByKey(String userId,String deviceUniqueId,String appName) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("user", userId)
	        .addValue("deviceUniqueId", deviceUniqueId).addValue("appName", appName);
		try{
		return jdbcTemplate.queryForObject(getUserDefinedAppByKeySql, parameterSource,
				appBeanRowMapper);
		}catch(EmptyResultDataAccessException e){
		}
		return null;
	}
	
	@Override
	public List<GlobalSyncApp> getUserDefinedAppByIntersection(String userId,String deviceUniqueId) {
		return jdbcTemplate.query(getUserDefinedAppByIntersectionSql,
				new MapSqlParameterSource().addValue("user", userId).addValue("deviceUniqueId", deviceUniqueId),
				appBeanRowMapper);
	}
	
}
