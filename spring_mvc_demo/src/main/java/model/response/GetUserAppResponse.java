package model.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetUserAppResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonProperty("device_unique_id")
	private String deviceUniqueId;
	@JsonProperty("app_name")
	private String appName;
	@JsonProperty("syncable")
	private boolean syncable;
	@JsonProperty("uninstalled")
	private boolean uninstalled;
	@JsonProperty("folder_path")
	private List<String> folderPath;
	@JsonProperty("source_path")
	private String sourcePath;
	private String path;
	
	public String getDeviceUniqueId() {
		return deviceUniqueId;
	}

	public void setDeviceUniqueId(String deviceUniqueId) {
		this.deviceUniqueId = deviceUniqueId;
	}

	public String getAppName() {
		return appName;
	}

	public void setAppName(String appName) {
		this.appName = appName;
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


	public List<String> getFolderPath() {
		return folderPath;
	}

	public void setFolderPath(List<String> folderPath) {
		this.folderPath = folderPath;
	}

	public GetUserAppResponse() {
		super();
	}

	public String getSourcePath() {
		return sourcePath;
	}

	public void setSourcePath(String sourcePath) {
		this.sourcePath = sourcePath;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}


}
