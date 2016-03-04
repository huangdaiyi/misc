package model.response;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import model.CellPhone;

import org.springframework.jdbc.core.RowMapper;

import utils.DateUtils;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetUserResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private String id;
	@JsonProperty("display_name")
	private String displayName;
	private String name;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("last_edit_time")
	private String lastEditTime;
	private String settings;
	@JsonProperty("backup_size")
	private long backupSize;
	@JsonProperty("chat_usage")
	private long chatUsage;
	@JsonProperty("contact_usage")
	private long contactUsage;
	private long usage;
	private long total;
	private long available;
	@JsonProperty("unique_id")
	private String deviceUid;
	private List<CellPhone> cellphones;

	public GetUserResponse() {
		super();
	}

	@Deprecated
	public static RowMapper<GetUserResponse> getMapper() {
		return new RowMapper<GetUserResponse>() {
			@Override
			public GetUserResponse mapRow(ResultSet rs, int rowNum) throws SQLException {
				GetUserResponse userUsage = new GetUserResponse();
				userUsage.setTotal(rs.getLong("total"));
				userUsage.setLastEditTime(DateUtils.convertDatetime(rs.getLong("last_edit_time"), "yyyy-MM-dd HH:mm:ss"));
				userUsage.setSettings(rs.getString("settings"));
				userUsage.setId(rs.getString("name"));
				return userUsage;
			}
		};
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getLastEditTime() {
		return lastEditTime;
	}

	public void setLastEditTime(String lastEditTime) {
		this.lastEditTime = lastEditTime;
	}

	public String getSettings() {
		return settings;
	}

	public void setSettings(String settings) {
		this.settings = settings;
	}

	public long getBackupSize() {
		return backupSize;
	}

	public void setBackupSize(long backupSize) {
		this.backupSize = backupSize;
	}

	public long getChatUsage() {
		return chatUsage;
	}

	public void setChatUsage(long chatUsage) {
		this.chatUsage = chatUsage;
	}

	public long getContactUsage() {
		return contactUsage;
	}

	public void setContactUsage(long contactUsage) {
		this.contactUsage = contactUsage;
	}

	public long getUsage() {
		return usage;
	}

	public void setUsage(long usage) {
		this.usage = usage;
	}

	public long getTotal() {
		return total;
	}

	public void setTotal(long total) {
		this.total = total;
	}

	public long getAvailable() {
		return available;
	}

	public void setAvailable(long available) {
		this.available = available;
	}

	public String getDeviceUid() {
		return deviceUid;
	}

	public void setDeviceUid(String deviceUid) {
		this.deviceUid = deviceUid;
	}

	public List<CellPhone> getCellphones() {
		return cellphones;
	}

	public void setCellphones(List<CellPhone> cellphones) {
		this.cellphones = cellphones;
	}

}
