package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetGlobalAppResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonProperty("app_name")
	private String appName;
	@JsonProperty("folder_path")
	private String folderPath;


	public String getAppName() {
		return appName;
	}


	public void setAppName(String appName) {
		this.appName = appName;
	}



	public String getFolderPath() {
		return folderPath;
	}


	public void setFolderPath(String folderPath) {
		this.folderPath = folderPath;
	}


	public GetGlobalAppResponse() {
		super();
	}


}
