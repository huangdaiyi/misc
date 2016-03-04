package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetFolderBackupCountResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonProperty("backup_count")
	private int backupCount;

	public GetFolderBackupCountResponse() {
		super();
	}

	public int getBackupCount() {
		return backupCount;
	}

	public void setBackupCount(int backupCount) {
		this.backupCount = backupCount;
	}

}
