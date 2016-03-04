package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonProperty;

public class GetExtensionBackupResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private Integer id;
	private String extension;
	@JsonProperty("backup_count")
	private int backupCount;
	
	public GetExtensionBackupResponse() {
		super();
	}

	
	public Integer getId() {
		return id;
	}


	public void setId(Integer id) {
		this.id = id;
	}


	public String getExtension() {
		return extension;
	}

	public void setExtension(String extension) {
		this.extension = extension;
	}

	public int getBackupCount() {
		return backupCount;
	}

	public void setBackupCount(int backupCount) {
		this.backupCount = backupCount;
	}

	
}
