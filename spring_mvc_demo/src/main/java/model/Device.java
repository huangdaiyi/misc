package model;

import java.io.Serializable;

import model.response.GetDeviceResponse;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonInclude(Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true, value = { "getStatus", "getGoogleRegisterId" })
public class Device implements Serializable {
	private static final long serialVersionUID = 1L;

	private int id;
	@JsonProperty("user_id")
	private String userId;
	@JsonProperty("device_id")
	private String deviceId;
	private String lastEditUser;
	private long lastEditTime;
	@JsonProperty("unique_id")
	private String deviceUid;
	private String settings;
	private String name;
	private String status;

	public Device() {
		super();
	}
	public GetDeviceResponse toGetDeviceResponse(){
		GetDeviceResponse getDeviceResponse = new GetDeviceResponse();
		getDeviceResponse.setDeviceUid(getDeviceUid());
		getDeviceResponse.setId(getId());
		getDeviceResponse.setName(getName());
		getDeviceResponse.setSettings(getSettings());
		return getDeviceResponse;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getDeviceId() {
		return deviceId;
	}

	public void setDeviceId(String deviceId) {
		this.deviceId = deviceId;
	}

	public String getLastEditUser() {
		return lastEditUser;
	}

	public void setLastEditUser(String lastEditUser) {
		this.lastEditUser = lastEditUser;
	}

	public long getLastEditTime() {
		return lastEditTime;
	}

	public void setLastEditTime(long lastEditTime) {
		this.lastEditTime = lastEditTime;
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
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getStatus() {
		return status;
	}
	public void setStatus(String status) {
		this.status = status;
	}

}
