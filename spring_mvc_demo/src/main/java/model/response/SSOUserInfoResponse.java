package model.response;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import model.CellPhone;
import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SSOUserInfoResponse implements Serializable {

	private static final long serialVersionUID = 1;

	private String status;
	private String id;
	private String name;
	@JsonProperty("display_name")
	private String displayName;
	private List<CellPhone> cellphones;
	@JsonProperty("device_id")
	private String deviceId;
	private String expired;
	@JsonProperty("profile_image")
	private String profileImage;
	@JsonProperty("inactive_cellphone")
	private List<Map<String, Object>> inactiveCellphone;
	private String error;
	private List<String> emails;

	public SSOUserInfoResponse() {
		super();
	}

	public RequestBase toRequestBase(String token) {
		RequestBase requestBase = new RequestBase();
		requestBase.setToken(token);
		requestBase.setUserId(id);
		requestBase.setDeviceId(deviceId);
		return requestBase;
	}

	public GetUserResponse toUser() {
		GetUserResponse user = new GetUserResponse();
		user.setId(getId());
		user.setName(getName());
		user.setDisplayName(getDisplayName());
		user.setCellphones(getCellphones());
		return user;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDisplayName() {
		return displayName;
	}
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public List<CellPhone> getCellphones() {
		return cellphones;
	}

	public void setCellphones(List<CellPhone> cellphones) {
		this.cellphones = cellphones;
	}

	public String getDeviceId() {
		return deviceId;
	}

	public void setDeviceId(String deviceId) {
		this.deviceId = deviceId;
	}

	public String getExpired() {
		return expired;
	}

	public void setExpired(String expired) {
		this.expired = expired;
	}

	public String getProfileImage() {
		return profileImage;
	}

	public void setProfileImage(String profileImage) {
		this.profileImage = profileImage;
	}

	public List<Map<String, Object>> getInactiveCellphone() {
		return inactiveCellphone;
	}

	public void setInactiveCellphone(List<Map<String, Object>> inactiveCellphone) {
		this.inactiveCellphone = inactiveCellphone;
	}

	public String getError() {
		return error;
	}

	public void setError(String error) {
		this.error = error;
	}

	public List<String> getEmails() {
		return emails;
	}

	public void setEmails(List<String> emails) {
		this.emails = emails;
	}

}
