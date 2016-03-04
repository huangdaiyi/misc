package model;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class User implements Serializable {

	private static final long serialVersionUID = 1;

	@JsonProperty("id")
	private String userId;
	@JsonProperty("name")
	private String userName;
	@JsonProperty("display_name")
	private String userDisplayName;
	@JsonProperty("cellphones")
	private List<Map<String, Object>> cellphones;
	private String usage;
	@JsonProperty("modified_at")
	private String modifiedAt;
	private Long total;
	private String backupSize;
	private String settings;
	@JsonProperty("unique_id")
	private List<String> deviceUid;

	public User() {
		super();
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getUserDisplayName() {
		return userDisplayName;
	}

	public void setUserDisplayName(String userDisplayName) {
		this.userDisplayName = userDisplayName;
	}

	public List<Map<String, Object>> getCellphones() {
		return cellphones;
	}

	public void setCellphones(List<Map<String, Object>> cellphones) {
		this.cellphones = cellphones;
	}

	public String getUsage() {
		return usage;
	}

	public void setUsage(String usage) {
		this.usage = usage;
	}

	public String getModifiedAt() {
		return modifiedAt;
	}

	public void setModifiedAt(String modifiedAt) {
		this.modifiedAt = modifiedAt;
	}

	public Long getTotal() {
		return total;
	}

	public void setTotal(Long total) {
		this.total = total;
	}

	public String getBackupSize() {
		return backupSize;
	}

	public void setBackupSize(String backupSize) {
		this.backupSize = backupSize;
	}

	public String getSettings() {
		return settings;
	}

	public void setSettings(String settings) {
		this.settings = settings;
	}

	public List<String> getDeviceUid() {
		return deviceUid;
	}

	public void setDeviceUid(List<String> deviceUid) {
		this.deviceUid = deviceUid;
	}

}
