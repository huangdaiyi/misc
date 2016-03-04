package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SSOFindDeviceNameStatusByDeviceIdResponse implements Serializable {
	private static final long serialVersionUID = 1;
	
	private String status;
	@JsonProperty("device_status")
	private String deviceStatus;
	@JsonProperty("device_name")
	private String deviceName;
	
	public SSOFindDeviceNameStatusByDeviceIdResponse() {
		super();
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getDeviceStatus() {
		return deviceStatus;
	}

	public void setDeviceStatus(String deviceStatus) {
		this.deviceStatus = deviceStatus;
	}

	public String getDeviceName() {
		return deviceName;
	}

	public void setDeviceName(String deviceName) {
		this.deviceName = deviceName;
	}
}
