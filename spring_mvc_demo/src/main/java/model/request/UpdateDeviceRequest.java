package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UpdateDeviceRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("unique_id")
	private String deviceUid;
	private String settings;

	public UpdateDeviceRequest() {
		super();
	}

	public String getDeviceUid() {
		return deviceUid;
	}

	public void setDeviceUid(String deviceUid) {
		this.deviceUid = deviceUid;
	}

	public String getSettings() {
		return settings;
	}

	public void setSettings(String settings) {
		this.settings = settings;
	}

}
