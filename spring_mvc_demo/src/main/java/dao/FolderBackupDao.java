package dao;

public interface FolderBackupDao {

	public String getFolderBackupIdByCriteria(String parentId, String name, String deviceUid, String fullSourcePath);

}
