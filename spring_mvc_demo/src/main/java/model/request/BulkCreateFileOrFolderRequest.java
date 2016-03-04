package model.request;

import java.util.ArrayList;

import model.IRequestBase;
import model.response.BulkCreateFileOrFolderErrorResponse;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkCreateFileOrFolderRequest extends ArrayList<CreateFileOrFolderRequest> implements IRequestBase {
	private static final long serialVersionUID = 1;

	private String token;
	private String userId;
	private String deviceId;
	private String deviceUid;
	private String dbHost;
	private int dbPort;
	private String dbInstance;
	private String dbServerName;
	
	public BulkCreateFileOrFolderErrorResponse toErrorResponse(int index) {
		BulkCreateFileOrFolderErrorResponse response = new BulkCreateFileOrFolderErrorResponse();
		CreateFileOrFolderRequest item = this.get(index);
		response.setIndex(index);
		response.setFolder(item.isFolder());
		response.setPath(item.getPath());
		response.setSourcePath(item.getSourcePath());
		response.setOwnerId(item.getOwnerId());
		return response;
	}
	
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
	public String getDeviceUid() {
		return deviceUid;
	}
	public void setDeviceUid(String deviceUid) {
		this.deviceUid = deviceUid;
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
