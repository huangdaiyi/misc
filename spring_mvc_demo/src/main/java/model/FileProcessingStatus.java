package model;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import constants.ProcessAction;
import constants.ProcessingStatus;

public class FileProcessingStatus {
	
	private String metadataIndexId;
	private ProcessingStatus processingStatus;
	private float percentage;
	private String itemInProgress;
	private ProcessAction processAction;
	
	@Deprecated
	public static RowMapper<FileProcessingStatus> getMapper(){
		return new RowMapper<FileProcessingStatus>() {
			@Override
			public FileProcessingStatus mapRow(ResultSet rs, int rowNum) throws SQLException {
				FileProcessingStatus fileProcessingStatus = new FileProcessingStatus();
				fileProcessingStatus.setMetadataIndexId(rs.getString("metadata_index_id"));
				fileProcessingStatus.setProcessingStatus(ProcessingStatus.parse(rs.getInt("processing_status")));
				fileProcessingStatus.setPercentage(rs.getFloat("percentage"));
				fileProcessingStatus.setItemInProgress(rs.getString("item_in_progress"));
				fileProcessingStatus.setProcessAction(ProcessAction.parse(rs.getString("process_action")));				return fileProcessingStatus;
			}
		};
	}
	
	public FileProcessingStatus() {
		super();
	}

	public FileProcessingStatus(String metadataIndexId,
			ProcessingStatus processingStatus, float percentage,
			String itemInProgress, ProcessAction processAction) {
		super();
		this.metadataIndexId = metadataIndexId;
		this.processingStatus = processingStatus;
		this.percentage = percentage;
		this.itemInProgress = itemInProgress;
		this.processAction = processAction;
	}
	
	public boolean isUnderProcessing() {
		if (this.processingStatus == ProcessingStatus.PROCESSING 
				|| this.processingStatus == ProcessingStatus.WAITING
				|| this.processingStatus == ProcessingStatus.LOCKING) {
			return true;
		}
		return false;
	}
	
	public String getMetadataIndexId() {
		return metadataIndexId;
	}
	
	public void setMetadataIndexId(String metadataIndexId) {
		this.metadataIndexId = metadataIndexId;
	}
	
	public ProcessingStatus getProcessingStatus() {
		return processingStatus;
	}
	
	public void setProcessingStatus(ProcessingStatus processingStatus) {
		this.processingStatus = processingStatus;
	}
	
	public float getPercentage() {
		return percentage;
	}
	
	public void setPercentage(float percentage) {
		this.percentage = percentage;
	}
	
	public String getItemInProgress() {
		return itemInProgress;
	}
	
	public void setItemInProgress(String itemInProgress) {
		this.itemInProgress = itemInProgress;
	}
	
	public ProcessAction getProcessAction() {
		return processAction;
	}
	
	public void setProcessAction(ProcessAction processAction) {
		this.processAction = processAction;
	}
}
