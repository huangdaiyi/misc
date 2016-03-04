package model;

import java.io.Serializable;
import java.util.List;

import model.response.GetGlobalAppResponse;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GlobalSyncApp implements Serializable {
	private static final long serialVersionUID = 1L;

    private String user;
	private String appName;
	private String singleFolderPath;
	private List<String> folderPath;
	private String deviceUniqueId;
	private boolean syncable;
	private boolean uninstalled;

	public GlobalSyncApp() {
		super();
	}
	
	public GetGlobalAppResponse toGetGlobalAppResponse(){
		GetGlobalAppResponse globalAppResponse = new GetGlobalAppResponse();
		globalAppResponse.setAppName(getAppName());
		globalAppResponse.setFolderPath(getSingleFolderPath());
		return globalAppResponse;		
	}

	public String getUser() {
		return user;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getAppName() {
		return appName;
	}

	public void setAppName(String appName) {
		this.appName = appName;
	}

	public String getDeviceUniqueId() {
		return deviceUniqueId;
	}

	public void setDeviceUniqueId(String deviceUniqueId) {
		this.deviceUniqueId = deviceUniqueId;
	}

	public boolean isSyncable() {
		return syncable;
	}

	public void setSyncable(boolean syncable) {
		this.syncable = syncable;
	}

	public boolean isUninstalled() {
		return uninstalled;
	}

	public void setUninstalled(boolean uninstalled) {
		this.uninstalled = uninstalled;
	}

	public String getSingleFolderPath() {
		return singleFolderPath;
	}

	public void setSingleFolderPath(String singleFolderPath) {
		this.singleFolderPath = singleFolderPath;
	}

	public List<String> getFolderPath() {
		return folderPath;
	}

	public void setFolderPath(List<String> folderPath) {
		this.folderPath = folderPath;
	}


}