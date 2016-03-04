package dao.impl;

import java.sql.Types;
import java.util.List;

import model.ReaderFileMetadata;
import model.ReaderPageDetail;
import model.SourceTargetMap;

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
import org.springframework.transaction.annotation.Transactional;

import dao.ReaderFileMetadataDao;

@Repository
public class ReaderFileMetadataDaoImpl implements ReaderFileMetadataDao {

	@Value("${sql.copy_file_processing_status}")
	private String copyFileProcessingStatusSql;
	@Value("${sql.copy_reader_file_metadata}")
	private String copyReaderFileMetadataSql;
	@Value("${sql.copy_reader_page_detail}")
	private String copyReaderPageDetailSql;
	@Value("${sql.get_reader_file_metadata}")
	private String getReaderFileMetadataSql;
	@Value("${sql.get_reader_backup_file_metadata}")
	private String getReaderBackupFileMetadataSql;
	@Value("${sql.get_reader_page_details}")
	private String getReaderPageDetailsSql;
	@Value("${sql.replace_reader_file_metadata}")
	private String replaceReaderFileMetadataSql;
	@Value("${sql.replace_reader_page_detail}")
	private String replaceReaderPageDetailSql;
	@Value("${sql.delete_reader_file_metadata}")
	private String deleteReaderFileMetadataSql;
	@Value("${sql.delete_reader_page_details}")
	private String deleteReaderPageDetailsSql;

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	// bean row mapper
	private BeanPropertyRowMapper<ReaderFileMetadata> readerFileMetadataBeanRowMapper = new BeanPropertyRowMapper<ReaderFileMetadata>(ReaderFileMetadata.class);
	private BeanPropertyRowMapper<ReaderPageDetail> readerPageDetailBeanRowMapper = new BeanPropertyRowMapper<ReaderPageDetail>(ReaderPageDetail.class);

	@Override
	public void copyFileProcessingStatus(String sourceId, String targetId) {
		jdbcTemplate.update(copyFileProcessingStatusSql, new MapSqlParameterSource().addValue("sourceId", sourceId, Types.VARCHAR).addValue("targetId", targetId, Types.VARCHAR));
	}

	@Override
	public void copyReaderFileMetadata(String sourceId, String targetId) {
		jdbcTemplate.update(copyReaderFileMetadataSql, new MapSqlParameterSource().addValue("sourceId", sourceId, Types.VARCHAR).addValue("targetId", targetId, Types.VARCHAR));
	}

	@Override
	public void copyReaderFileMetadataDetail(String sourceId, String targetId) {
		jdbcTemplate.update(copyReaderPageDetailSql, new MapSqlParameterSource().addValue("sourceId", sourceId, Types.VARCHAR).addValue("targetId", targetId, Types.VARCHAR));
	}

	@Override
	public void copyFileProcessingStatus(List<SourceTargetMap> maps) {
		batchUpdateBySource(maps, copyFileProcessingStatusSql);
	}

	@Override
	public void copyReaderFileMetadata(List<SourceTargetMap> maps) {
		batchUpdateBySource(maps, copyReaderFileMetadataSql);
	}

	@Override
	public void copyReaderFileMetadataDetail(List<SourceTargetMap> maps) {
		batchUpdateBySource(maps, copyReaderPageDetailSql);
	}


	private void batchUpdateBySource(List<SourceTargetMap> maps, String sql) {
		SqlParameterSource[] params = SqlParameterSourceUtils.createBatch(maps.toArray());
		jdbcTemplate.batchUpdate(sql, params);
	}
	
	
	
	@Override
	public ReaderFileMetadata getReaderFileMetadata(String metadataIndexId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("metadataIndexId", metadataIndexId);
		//paramSource.addValue("backup", isBackup);
		try {
			return jdbcTemplate.queryForObject(getReaderFileMetadataSql, paramSource, readerFileMetadataBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}
	
	
	@Override
	public ReaderFileMetadata getReaderBackupFileMetadata(String metadataIndexId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("metadataIndexId", metadataIndexId);
		try {
			return jdbcTemplate.queryForObject(getReaderBackupFileMetadataSql, paramSource, readerFileMetadataBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public List<ReaderPageDetail> getReaderPageDetails(String metadataIndexId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("metadataIndexId", metadataIndexId);
		//paramSource.addValue("backup", isBackup);
		return jdbcTemplate.query(getReaderPageDetailsSql, paramSource, readerPageDetailBeanRowMapper);
	}

	@Override
	public ReaderFileMetadata replaceReaderFileMetadata(ReaderFileMetadata readerFileMetadata) {
		jdbcTemplate.update(replaceReaderFileMetadataSql, new BeanPropertySqlParameterSource(readerFileMetadata));
		return readerFileMetadata;
	}

	@Override
	@Transactional
	public List<ReaderPageDetail> replaceReaderPageDetail(String metadataIndexId, List<ReaderPageDetail> readerPageDetails) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("metadataIndexId", metadataIndexId);
		paramSource.addValue("backup", false);
		jdbcTemplate.update(deleteReaderPageDetailsSql, paramSource);
		if (readerPageDetails == null) {
			return readerPageDetails;
		}
		for (ReaderPageDetail readerPageDetail : readerPageDetails) {
			readerPageDetail.setMetadataIndexId(metadataIndexId);
			jdbcTemplate.update(replaceReaderPageDetailSql, new BeanPropertySqlParameterSource(readerPageDetail));
		}
		return readerPageDetails;
	}

	@Override
	public void deleteReaderFileMetadataAndPageDetails(String metadataIndexId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("metadataIndexId", metadataIndexId);
		//paramSource.addValue("backup", false);
		jdbcTemplate.update(deleteReaderFileMetadataSql, paramSource);
		jdbcTemplate.update(deleteReaderPageDetailsSql, paramSource);
	}

}
