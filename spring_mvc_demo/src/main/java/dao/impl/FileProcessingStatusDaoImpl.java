package dao.impl;

import model.FileProcessingStatus;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;

import constants.HttpStatus;
import dao.FileProcessingStatusDao;
import exception.MetadataException;

@Repository
public class FileProcessingStatusDaoImpl implements FileProcessingStatusDao {

	@Value("${sql.create_file_processing_status}")
	private String createFileProcessingStatus;
	@Value("${sql.get_file_processing_status}")
	private String getFileProcessingStatus;
	
	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	@Override
	public void updateFileProcessingStatus(FileProcessingStatus fileProcessingStatus) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("metadataIndexId", fileProcessingStatus.getMetadataIndexId());
		paramSource.addValue("processingStatus", fileProcessingStatus.getProcessingStatus().getProcessingStatus());
		paramSource.addValue("percentage", fileProcessingStatus.getPercentage());
		paramSource.addValue("itemInProgress", fileProcessingStatus.getItemInProgress());
		paramSource.addValue("processAction", fileProcessingStatus.getProcessAction().getProcessAction());
		try {
			jdbcTemplate.update(createFileProcessingStatus, paramSource);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public FileProcessingStatus getFileProcessingStatus(String metadataIndexId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("metadataIndexId", metadataIndexId);
		try {
			return jdbcTemplate.queryForObject(getFileProcessingStatus, paramSource, FileProcessingStatus.getMapper());
		} catch (EmptyResultDataAccessException e) {
			return null;
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

}
