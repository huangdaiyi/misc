package service.impl;

import model.BackupMetadata;
import model.FileProcessingStatus;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import service.FileProcessingStatusService;
import dao.BackupMetadataDao;
import dao.FileProcessingStatusDao;

@Service
public class FileProcessingStatusServiceImpl implements FileProcessingStatusService {
	
	@Autowired
	private FileProcessingStatusDao fileProcessingStatusDao;
	@Autowired
	private BackupMetadataDao backupMetadataDao;
	
	public void updateFileProcessingStatus(FileProcessingStatus fileProcessingStatus) {
		fileProcessingStatusDao.updateFileProcessingStatus(fileProcessingStatus);
		BackupMetadata backup = backupMetadataDao.getLatestBackup(fileProcessingStatus.getMetadataIndexId());
		if (backup != null) {
			fileProcessingStatus.setMetadataIndexId(backup.getId());
			fileProcessingStatusDao.updateFileProcessingStatus(fileProcessingStatus);
		}
	}
}
