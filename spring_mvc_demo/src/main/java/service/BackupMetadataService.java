package service;

import java.util.List;

import model.BackupMetadata;
import model.Metadata;
import exception.MetadataException;

public interface BackupMetadataService {


	public List<BackupMetadata> backupMetadata(List<Metadata> metadatas, String ownerId) throws MetadataException;
	
	public List<BackupMetadata> backupMetadata(Metadata metadata, String ownerId) throws MetadataException;
	
	public List<BackupMetadata> backupMetadata(List<Metadata> metadatas, String ownerId , String firstExistParentId) throws MetadataException;
	
	//public String unbackupFile(String originalIndexId) throws MetadataException;
	
	public void deleteBackup(String backupMetadataId) throws MetadataException;
	
//	public void deleteBackups(List<String> backupMetadataIds);
	
	public void deleteAllBackup(String ownerId);
	
	public BackupMetadata updateBackup(Metadata metadata); 
	
	public String getBackupIndexByPath(String fullPath, String ownerId);
	
	public BackupMetadata registerRootBackup(BackupMetadata root);
	
	public List<String> getParentIds(String id);
	
	public void deleteFolderAllBackup(String ownerId, String folderId);
	
	public void deleteFolderBackup(String ownerId, String folderId);
	
	public void renameBackup(Metadata metadata);

	public List<BackupMetadata> getAllSubFoldersAndFiles(BackupMetadata backupMetadata);
	
	public List<BackupMetadata> ExistBackupName(String metadataName, String backupParentId, String sourcePath);
	
	public void deleteFileOldBackups(Metadata metadata);

	public List<Metadata> getBackupCountByExtension(String ownerId,List<Metadata> metadatas);

	public int getBackupCount(Metadata file);
	
	public int getBackupCount(Metadata file, List<Metadata> metadatas);
	
	public void updateBackupCount(Metadata metadata, int count);

	public void unbackupMetadata(String originalIndexId) throws MetadataException;

	public void unbackupMetadatas(List<String> originalIndexIds)throws MetadataException;

	public void createSingleBackup(Metadata metadata, String userId);

}