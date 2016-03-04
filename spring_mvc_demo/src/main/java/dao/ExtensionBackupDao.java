package dao;

import java.util.List;

import model.DefaultBackupSetting;

public interface ExtensionBackupDao {

	public List<DefaultBackupSetting> getExtensionBackup(String userId);
	
	public void addExtensionBackup(String userId, String extension);
	
	public void deleteExtensionBackup(String userId, List<Integer> idList);

	public void batchupdateExtensionBackup(List<DefaultBackupSetting> updateUserExtensionbackups);
	
}
