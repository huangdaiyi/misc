package model.request;

import java.util.List;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UpdateUserAppRequest extends RequestBase {

	private static final long serialVersionUID = 1L;
	@JsonProperty("device_unique_id")
	private String deviceUniqueId;
	@JsonProperty("app_name")
	private List<String> appName;
	
	public UpdateUserAppRequest() {
		super();
	}

	public String getDeviceUniqueId() {
		return deviceUniqueId;
	}

	public void setDeviceUniqueId(String deviceUniqueId) {
		this.deviceUniqueId = deviceUniqueId;
	}

	public List<String> getAppName() {
		return appName;
	}

	public void setAppName(List<String> appName) {
		this.appName = appName;
	}


}
