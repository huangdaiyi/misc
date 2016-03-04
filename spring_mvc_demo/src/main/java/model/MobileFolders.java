package model;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import constants.SyncType;

public class MobileFolders implements Serializable {

	private static final long serialVersionUID = 1L;

	private int id;
	private String userId;
	private String fullSourcePath;
	private SyncType status;
	private String deviceUid;

	public MobileFolders() {
		super();
	}
	
	@Deprecated
	public static RowMapper<MobileFolders> getMapper(){
		return new RowMapper<MobileFolders>() {
			@Override
			public MobileFolders mapRow(ResultSet rs, int rowNum) throws SQLException {
				MobileFolders mobileFolders = new MobileFolders();
				mobileFolders.setId(rs.getInt("id"));
				mobileFolders.setUserId(rs.getString("user_id"));
				mobileFolders.setFullSourcePath(rs.getString("full_source_path"));
				mobileFolders.setDeviceUid(rs.getString("device_uid"));
				mobileFolders.setStatus(SyncType.parse(rs.getString("status")));
				return mobileFolders;
			}
		};
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

	public String getFullSourcePath() {
		return fullSourcePath;
	}

	public void setFullSourcePath(String fullSourcePath) {
		this.fullSourcePath = fullSourcePath;
	}

	public SyncType getStatus() {
		return status;
	}

	public void setStatus(SyncType status) {
		this.status = status;
	}

	public String getDeviceUid() {
		return deviceUid;
	}

	public void setDeviceUid(String deviceUid) {
		this.deviceUid = deviceUid;
	}

}
