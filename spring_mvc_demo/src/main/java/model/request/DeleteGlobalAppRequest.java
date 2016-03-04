package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DeleteGlobalAppRequest extends RequestBase {
	private static final long serialVersionUID = 1;
	@JsonProperty("app_name")
	private String appName;
	public String getAppName() {
		return appName;
	}
	public void setAppName(String appName) {
		this.appName = appName;
	}
	
}