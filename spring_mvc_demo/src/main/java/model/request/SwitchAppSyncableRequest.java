package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SwitchAppSyncableRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("app_name")
	private String appName;
	@JsonProperty("device_unique_id")
	private String deviceUniqueId;
	private boolean syncable;

	public String getDeviceUniqueId() {
		return deviceUniqueId;
	}

	public void setDeviceUniqueId(String deviceUniqueId) {
		this.deviceUniqueId = deviceUniqueId;
	}

	public SwitchAppSyncableRequest() {
		super();
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


}
