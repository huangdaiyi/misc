package dao;

import model.FileProcessingStatus;

public interface FileProcessingStatusDao {

	public void updateFileProcessingStatus(FileProcessingStatus fileProcessingStatus);
	
	public FileProcessingStatus getFileProcessingStatus(String metadataIndexId);
}
