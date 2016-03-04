package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BackupCountResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonProperty("backup_count")
	private int backupCount;

	public BackupCountResponse() {
		super();
	}

	public int getBackupCount() {
		return backupCount;
	}

	public void setBackupCount(int backupCount) {
		this.backupCount = backupCount;
	}

}