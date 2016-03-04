package dao.impl;

import model.response.ClientVersionResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;

import constants.HttpStatus;
import dao.ClientVersionDao;
import exception.MetadataException;


@Repository
public class ClientVersionDaoImpl implements ClientVersionDao {
	
	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;
	
	@Value("${sql.get_client_version}")
	private String getClientVersionSql;
	
	@Value("${sql.update_client_version}")
	private String updateClientVersionSql;
	
	private BeanPropertyRowMapper<ClientVersionResponse> clientVersionResponseBeanRowMapper = new BeanPropertyRowMapper<ClientVersionResponse>(
			ClientVersionResponse.class);
	
	@Override
	public ClientVersionResponse getClientVersion(String platform) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("platform", platform);
		try {
			return jdbcTemplate.queryForObject(
					getClientVersionSql, parameterSource,
					clientVersionResponseBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}
	
	@Override
	public void updateClientVersion(String platform, String version,
			String updateType, String downloadUrl) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("platform", platform)
				.addValue("version", version)
				.addValue("updateType", updateType)
				.addValue("downloadUrl", downloadUrl);
		try {
			jdbcTemplate.update(updateClientVersionSql, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
		
	}

}
