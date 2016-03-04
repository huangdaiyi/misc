package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BackupResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	@JsonProperty("last_edit_time")
	private long modifiedAt;

	public BackupResponse() {
		super();
	}
	
	public BackupResponse(String name, long modifiedAt) {
		this.name = name;
		this.modifiedAt = modifiedAt;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public long getModifiedAt() {
		return modifiedAt;
	}

	public void setModifiedAt(long modifiedAt) {
		this.modifiedAt = modifiedAt;
	}
	
}
