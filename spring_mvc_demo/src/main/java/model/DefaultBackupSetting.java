package model;

import java.io.Serializable;

import model.response.GetExtensionBackupResponse;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DefaultBackupSetting implements Serializable {
	private static final long serialVersionUID = 1L;
	@JsonProperty("id")
	private int id;
	private String extension;
	private String user;
	@JsonProperty("backup_count")
	private int backupCount;
	
	public GetExtensionBackupResponse toGetExtensionBackupResponse(){
		GetExtensionBackupResponse getExtensionBackupResponse = new GetExtensionBackupResponse();
		getExtensionBackupResponse.setId(this.getId());
		getExtensionBackupResponse.setExtension(this.extension);
		getExtensionBackupResponse.setBackupCount(this.getBackupCount());
		return getExtensionBackupResponse;
	}
	
	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}
	public String getExtension() {
		return extension;
	}
	public void setExtension(String extension) {
		this.extension = extension;
	}
	public String getUser() {
		return user;
	}
	public void setUser(String user) {
		this.user = user;
	}
	public int getBackupCount() {
		return backupCount;
	}
	public void setBackupCount(int backupCount) {
		this.backupCount = backupCount;
	}

	
	
}