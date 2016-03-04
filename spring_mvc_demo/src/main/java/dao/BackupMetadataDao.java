package dao;

import java.util.List;

import model.BackupMetadata;
import model.BackupMetadata4Delete;
import model.BackupMetadata4Modify;
import model.BaseMetadata;

public interface BackupMetadataDao {

	public BackupMetadata createFolderBackup(BackupMetadata metadata);

	public BackupMetadata createFileBuckup(BackupMetadata metadata);

	public List<BackupMetadata> batchFolderCreateBackup(
			List<BackupMetadata> metadatas);

	public List<BackupMetadata> batchFileCreateBackup(
			List<BackupMetadata> metadatas);

	public List<BackupMetadata4Modify> bathUpdateBackupNo(
			List<BackupMetadata4Modify> modifies);

	public List<BackupMetadata> getBackupByOriginalId(String originalId);

	public BackupMetadata getBackup(String id);

	public void deleteBackup(String originalId, int backupNo);

	public void deleteBackupById(String backupMedataIndexId);

	public void deleteBackupByIds(List<String> backupMedataIndexIds);

	public void batchDeleteBackup(List<BackupMetadata4Delete> deletions);

	public List<BackupMetadata> getBackups(List<String> originalIds);

	public String cancelBackupFile(String originalIndexId);

	public void clearBackup(String ownerId);

	public String getRootId(String ownerId);

	public BackupMetadata getLatestBackup(String originalIndexId);

	public BackupMetadata updateBackup(BackupMetadata backupMetadata);

	public List<BackupMetadata> batchUpdateBackup(
			List<BackupMetadata> backupMetadatas);

	public String getBackupParentId(String id);

	public List<String> getAllUpperIds(String id);

	public long countSubBackupsByParentId(String parentId, boolean isFolder);

	public String getBackupIndexId(String ownerId, String name, String parentId);

	public List<BackupMetadata> getSubFolder(String parentId);

	public List<BackupMetadata> getAllUnbackupMetadatas(String ownerId);

	public List<BackupMetadata> getUnbackupMetadatas(String ownerId,
			String parentId);

	public void updateBackupNote(String id, String note, long modifiedAt,
			String userId);

	// public BackupMetadata getBackupMetadataByCriteria(String parentId, String
	// name, String deviceUid, String fullSourcePath, boolean isFolder);

	public BackupMetadata getBackupFileByCriteria(String parentId, String name,
			String deviceUid, String fullSourcePath);

	public BackupMetadata getBackupFolderByCriteria(String parentId,
			String name, String deviceUid, String fullSourcePath);

	public BackupMetadata getBackupMetadataByPath(String path,
			String deviceUid, String fullSourcePath, boolean isFolder);

	public List<BackupMetadata> getBackupMetadatasByParentId(String parentId);

	public List<BackupMetadata> getAllBackupMetadatasByDeviceUid(
			String deviceUid);

	public List<BackupMetadata> ExistBackupName(String metadataName,
			String backupParentId, String sourcePath);

	void batchCancelBackupFile(List<String> ids);

	public void renameLinkFolder(String oldFolderFullSourcePath,
			String newFolderFullSourcePath);

	public String unbackupByBackupCount(String originalIndexId, int backupCount);

	public List<? extends BaseMetadata> getBackupMetadatasByPath(
			String[] pathFragmenets, String deviceUid, String fullSourcePath,
			int leave, boolean isFolder);

	public void cancelBackupFile(List<String> originalIndexIds);

	public void deleteBackupsByOriginalIds(List<String> originalIds);

	public void batchUnbackup(List<BackupMetadata> unbackupBackupMetadatas);

	public BackupMetadata getRoot(String ownerId);
	
	public List<? extends BaseMetadata> getBackupMetadatasBySearchingNameAndNote(String ownerId, String keyword);
	
	public List<BackupMetadata> getSubBackupByParentsAndNames(String ownerId, List<String> parentIds, List<String> names);
	
	public List<? extends BaseMetadata> getBackupMetadatasByBlockId(String ownerId, String blockId);

	public void updateBackupAsLinkFile(String id, String deviceUid, String fullSourcePath);
	
	public void updateBackupFileEncryption(String id, String blockId, boolean encrypted, long modifiedAt, String userId);

	public List<BackupMetadata> getBackupMetdatasByCriteria(String parentId, String name, String deviceUid, String fullSourcePath);
	
	public List<BackupMetadata> getUnBackupMetdatasByParentIds(List<String> parentIds);

}
