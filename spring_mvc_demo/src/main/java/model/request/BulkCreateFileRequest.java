package model.request;

import java.util.ArrayList;

import model.IRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkCreateFileRequest extends ArrayList<UpdateFileRequest> implements IRequestBase {
	
	private static final long serialVersionUID = 1;

	private String token;
	private String userId;
	private String deviceId;
	private String dbHost;
	private int dbPort;
	private String dbInstance;
	private String dbServerName;
	
	public String getToken() {
		return token;
	}
	public void setToken(String token) {
		this.token = token;
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
	public String getDbHost() {
		return dbHost;
	}
	public void setDbHost(String dbHost) {
		this.dbHost = dbHost;
	}
	public int getDbPort() {
		return dbPort;
	}
	public void setDbPort(int dbPort) {
		this.dbPort = dbPort;
	}
	public String getDbInstance() {
		return dbInstance;
	}
	public void setDbInstance(String dbInstance) {
		this.dbInstance = dbInstance;
	}
	public String getDbServerName() {
		return dbServerName;
	}
	public void setDbServerName(String dbServerName) {
		this.dbServerName = dbServerName;
	}
}
