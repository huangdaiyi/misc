package dao;

import java.util.List;

import model.SyncRelation;

public interface SyncRelationDao {

	public List<SyncRelation> getSyncRelationByCriteria(SyncRelation criteria);

	public void addSyncRelation(SyncRelation syncRelation);
	
	public void deleteSyncRelation(SyncRelation syncRelation);
	
	public void deleteSyncRelation(List<SyncRelation> syncRelations);
	
	public void updateSyncRelationEnable(long id, boolean enable);
	
	public List<SyncRelation> getRelationsOnFolderOfClient(String ownerId, String sourcePath);
	
	public List<SyncRelation> getUpperLevelFoldersOfClient(String ownerId, String sourcePath);
	
	public List<SyncRelation> getLowerLevelFoldersOfClient(String ownerId, String sourcePath);
	
	public List<SyncRelation> getLowerLevelFoldersOfClient(String ownerId, String sourcePath, boolean selfIncluded);
	
	public List<SyncRelation> getSyncRelationsByUserId(String userId);
	
	public void renameLinkFolder(String ownerId, String oldFolderSourcePath, String newFolderSourcePath);
}
