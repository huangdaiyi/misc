package model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SSOFindDeviceNameStatusByDeviceIdRequest implements Serializable {
	private static final long serialVersionUID = 1;
	
	@JsonProperty("device_id")
	private String deviceId;

	public SSOFindDeviceNameStatusByDeviceIdRequest() {
		super();
	}

	public String getDeviceId() {
		return deviceId;
	}

	public void setDeviceId(String deviceId) {
		this.deviceId = deviceId;
	}
}
