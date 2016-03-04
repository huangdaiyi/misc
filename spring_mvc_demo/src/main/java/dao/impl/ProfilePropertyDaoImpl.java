package dao.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import model.ProfileProperty;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.BeanPropertySqlParameterSource;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import dao.ProfilePropertyDao;

@Repository
public class ProfilePropertyDaoImpl implements ProfilePropertyDao {

	@Value("${sql.find_last_insert_id}")
	private String findLastInsertIdSql;
	@Value("${sql.create_profile}")
	private String createProfileSql;
	@Value("${sql.get_profile_id}")
	private String getProfileIdSql;
	@Value("${sql.delete_profile}")
	private String deleteProfileSql;
	@Value("${sql.replace_profile_property}")
	private String replaceProfilePropertySql;
	@Value("${sql.delete_profile_properties}")
	private String deleteProfilePropertiesSql;
	@Value("${sql.get_profile_property}")
	private String getProfilePropertySql;
	@Value("${sql.get_profile_properties}")
	private String getProfilePropertiesSql;
	@Value("${sql.get_profiles_by_metadata_index_id}")
	private String getProfilesByMetadataIndexIdSql;
	
//	@Value("${sql.create_profile_with_property}")
//	private String createProfileWithPropertySql;
	
	@Value("${sql.get_profile_properties_by_metaids}")
	private String getMultiProfilePropertiesSql;
	

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	private BeanPropertyRowMapper<ProfileProperty> profilePropertyRowMapper = new BeanPropertyRowMapper<ProfileProperty>(ProfileProperty.class);

	@Override
	public long getProfileId(String userId, String deviceUid, String metadataIndexId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("userId", userId);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("metadataIndexId", metadataIndexId);
		try {
			return jdbcTemplate.queryForObject(getProfileIdSql, paramSource, Long.class);
		} catch (EmptyResultDataAccessException e) {
		}
		return 0;
	}

	@Override
	@Transactional
	public ProfileProperty replaceProfileProperty(ProfileProperty profileProperty) {
		BeanPropertySqlParameterSource paramSource = new BeanPropertySqlParameterSource(profileProperty);
		// create profile if not exists
		long profileId = profileProperty.getProfileId();
		if (profileId <= 0) {
			profileId = getProfileId(profileProperty.getUserId(), profileProperty.getDeviceUid(), profileProperty.getMetadataIndexId());
		}
		if (profileId <= 0) {
			jdbcTemplate.update(createProfileSql, paramSource);
			profileId = jdbcTemplate.queryForObject(findLastInsertIdSql, (MapSqlParameterSource) null, Long.class);
		}
		profileProperty.setProfileId(profileId);
		jdbcTemplate.update(replaceProfilePropertySql, paramSource);
		return profileProperty;
	}
	
	@Override
	public List<ProfileProperty> getProfileProperties(String userId, String metadataIndexId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("userId", userId);
		parameterSource.addValue("metadataIndexId", metadataIndexId);
		return jdbcTemplate.query(getProfilePropertiesSql, parameterSource, new BeanPropertyRowMapper<ProfileProperty>(ProfileProperty.class));
	}

	@Override
	public ProfileProperty getProfileProperty(String userId, String deviceUid, String metadataIndexId, String propertyName) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("userId", userId);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("metadataIndexId", metadataIndexId);
		paramSource.addValue("propertyName", propertyName);
		try {
			return jdbcTemplate.queryForObject(getProfilePropertySql, paramSource, profilePropertyRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	@Transactional
	public void deleteProfileAndProperties(String userId, String deviceUid, String metadataIndexId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("userId", userId);
		paramSource.addValue("deviceUid", deviceUid);
		paramSource.addValue("metadataIndexId", metadataIndexId);
		jdbcTemplate.update(deleteProfileSql, paramSource);

		long profileId = getProfileId(userId, deviceUid, metadataIndexId);
		if (profileId <= 0) {
			return;
		}
		paramSource.addValue("profileId", profileId);
		jdbcTemplate.update(deleteProfilePropertiesSql, paramSource);
	}


	@Override
	public List<ProfileProperty> getMultiProfileProperties(String userId, Set<String> metadataIndexId) {
		if(metadataIndexId == null || metadataIndexId.size() == 0) {
			return new ArrayList<ProfileProperty>();
		}
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("userId", userId);
		parameterSource.addValue("metadataIndexIds", metadataIndexId);
		return jdbcTemplate.query(getMultiProfilePropertiesSql, parameterSource, new BeanPropertyRowMapper<ProfileProperty>(ProfileProperty.class));
	}
	
	@Override
	public void createProfileWithPropertys(ProfileProperty property){
		//jdbcTemplate.update(createProfileWithPropertySql, new BeanPropertySqlParameterSource(property));
	}
	
	@Override
	public void copyProfileProperty(String userId,  Map<String, String> maps) {
		
		List<ProfileProperty> profiles = getMultiProfileProperties(userId, maps.keySet());
		for (ProfileProperty profile : profiles) {
			profile.setMetadataIndexId(maps.get(profile.getMetadataIndexId()));
		}
		
		//jdbcTemplate.batchUpdate(createProfileWithPropertySql, SqlParameterSourceUtils.createBatch(profiles.toArray()));
		for (ProfileProperty profileProperty : profiles) {
			replaceProfileProperty(profileProperty);
		}
    }
}
