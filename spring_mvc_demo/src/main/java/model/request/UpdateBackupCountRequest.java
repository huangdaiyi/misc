package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateBackupCountRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;

	@JsonProperty("backup_count")
	private int backupCount;

	public UpdateBackupCountRequest() {
		super();
	}

	public int getBackupCount() {
		return backupCount;
	}

	public void setBackupCount(int backupCount) {
		this.backupCount = backupCount;
	}

}
