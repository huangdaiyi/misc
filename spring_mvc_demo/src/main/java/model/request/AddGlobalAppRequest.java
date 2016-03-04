package model.request;

import java.util.List;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class AddGlobalAppRequest extends RequestBase {

	private static final long serialVersionUID = 1L;
	@JsonProperty("app_name")
	private String appName;
	@JsonProperty("folder_path")
	private List<String> folderPath;
	
	public AddGlobalAppRequest() {
		super();
	}

	public String getAppName() {
		return appName;
	}

	public void setAppName(String appName) {
		this.appName = appName;
	}

	public List<String> getFolderPath() {
		return folderPath;
	}

	public void setFolderPath(List<String> folderPath) {
		this.folderPath = folderPath;
	}


}
