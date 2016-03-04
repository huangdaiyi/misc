package dao.impl;

import java.util.List;

import model.AlreadyRead;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;

import dao.AlreadyReadDao;

@Repository
public class AlreadyReadDaoImpl implements AlreadyReadDao {

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	@Value("${sql.update_already_read}")
	private String updateAlreadyReadSql;
	@Value("${sql.get_already_read}")
	private String getAlreadyReadSql;
	@Value("${sql.batch_get_already_read}")
	private String batchGetAlreadyReadSql;
	@Value("${sql.is_already_read}")
	private String isAlreadyReadSql;
	
	// bean row mapper
	private BeanPropertyRowMapper<AlreadyRead> alreadyReadBeanRowMapper = new BeanPropertyRowMapper<AlreadyRead>(
			AlreadyRead.class);
	
	@Override
	public void updateAlreadyRead(String metadataIndexId, String userId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("metadataIndexId", metadataIndexId)
		           .addValue("userId", userId);
		jdbcTemplate.update(updateAlreadyReadSql, paramSource);
	}
	
	@Override
	public List<AlreadyRead> getAlreadyRead(String metadataIndexId) {
		return jdbcTemplate.query(
				getAlreadyReadSql,
				new MapSqlParameterSource().addValue("metadataIndexId",metadataIndexId),
				alreadyReadBeanRowMapper);
	}
	
	@Override
	public List<AlreadyRead> batchGetAlreadyRead(List<String> metadataIndexIds) {
		if (metadataIndexIds.isEmpty()){
			return null;
		}
		return jdbcTemplate.query(
				batchGetAlreadyReadSql,
				new MapSqlParameterSource().addValue("metadataIndexIds",metadataIndexIds),
				alreadyReadBeanRowMapper);
	}
	
	@Override
	public boolean isAlreadyRead(String userId, String metadataIndexId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("userId", userId);
		paramSource.addValue("metadataIndexId", metadataIndexId);
		return jdbcTemplate.queryForObject(isAlreadyReadSql, paramSource, Boolean.class);
	}
}