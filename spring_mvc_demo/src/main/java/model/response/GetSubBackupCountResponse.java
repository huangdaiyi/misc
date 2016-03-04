package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetSubBackupCountResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	@JsonProperty("source_path")
	private String sourcePath;
	@JsonProperty("backup_count")
	private int backupCount;

	public GetSubBackupCountResponse() {
		super();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getSourcePath() {
		return sourcePath;
	}

	public void setSourcePath(String sourcePath) {
		this.sourcePath = sourcePath;
	}

	public int getBackupCount() {
		return backupCount;
	}

	public void setBackupCount(int backupCount) {
		this.backupCount = backupCount;
	}

}
