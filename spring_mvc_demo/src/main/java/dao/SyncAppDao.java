package dao;

import java.util.List;

import model.GlobalSyncApp;

public interface SyncAppDao {

	public void addGlobalApp(List<GlobalSyncApp> globalapps);

	public void deleteGlobalApp(String appName);

	public List<GlobalSyncApp> getGlobalAppByAppName(String appName);

	public List<GlobalSyncApp> getGlobalApp();

	public void addUserDefinedApp(List<GlobalSyncApp> globalApps);

	public void deleteUserDefinedApp(String userId,String appName,String deviceUniqueId);

	public void updateUserDefinedApp(List<GlobalSyncApp> globalApps);
	
	public GlobalSyncApp getUserDefinedAppByKey(String userId,String appName,String deviceUniqueId);

	public List<GlobalSyncApp> getUserDefinedAppByUser(String userId);

	public List<GlobalSyncApp> getUserDefinedAppByIntersection(String userId,String deviceUniqueId);

}
