package dao.impl;

import java.sql.Types;
import java.util.List;

import model.SourceTargetMap;
import model.TextViewSetting;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.BeanPropertySqlParameterSource;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSourceUtils;
import org.springframework.stereotype.Repository;

import dao.TextViewSettingDao;

@Repository
public class TextViewSettingDaoImpl implements TextViewSettingDao {
	
	@Value("${sql.get_text_view_setting}")
	private String getTextViewSettingSql;
	@Value("${sql.update_text_view_setting}")
	private String updateTextViewSettingSql;
	@Value("${sql.update_all_backups_text_view_setting}")
	private String updateAllBackupsTextViewSettingSql;
	@Value("${sql.copy_text_view_setting}")
	private String copyTextViewSettingSql;
	
	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;
	
	private BeanPropertyRowMapper<TextViewSetting> textViewSettingBeanRowMapper = 
			new BeanPropertyRowMapper<TextViewSetting>(TextViewSetting.class);

	@Override
	public TextViewSetting getTextViewSetting(String metadataIndexId,
			String viewerId, String viewerDeviceUniqueId) {
		
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("metadataIndexId", metadataIndexId);
		parameterSource.addValue("viewerId", viewerId);
		parameterSource.addValue("viewerDeviceUniqueId", viewerDeviceUniqueId);
		
		try {
			return jdbcTemplate.queryForObject(getTextViewSettingSql, parameterSource, textViewSettingBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
			return null;
		}
	}

	@Override
	public void updateTextViewSetting(TextViewSetting textViewSetting) {
		BeanPropertySqlParameterSource parameterSource = new BeanPropertySqlParameterSource(textViewSetting);
		jdbcTemplate.update(updateTextViewSettingSql, parameterSource);
	}
	
	@Override
	public void updateAllBackupsTextViewSetting(TextViewSetting textViewSetting) {
		BeanPropertySqlParameterSource parameterSource = new BeanPropertySqlParameterSource(textViewSetting);
		jdbcTemplate.update(updateAllBackupsTextViewSettingSql, parameterSource);
	}
	
	@Override
	public void copyTextViewSetting(String sourceId, String targetId) {
		jdbcTemplate.update(copyTextViewSettingSql, new MapSqlParameterSource()
		    .addValue("sourceId", sourceId, Types.VARCHAR)
		    .addValue("targetId", targetId, Types.VARCHAR));
	}
	
	@Override
	public void copyTextViewSettings(List<SourceTargetMap> maps) {
		jdbcTemplate.batchUpdate(copyTextViewSettingSql,SqlParameterSourceUtils.createBatch(maps.toArray()));
	}
	

}
