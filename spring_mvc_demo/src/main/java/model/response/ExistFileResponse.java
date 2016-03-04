package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ExistFileResponse implements Serializable {
	
	private static final long serialVersionUID = 1L;

	private String path;
	@JsonProperty("source_path")
	private String sourcePath;
	@JsonProperty("owner_id")
	private long ownerId;
	private boolean existed;
	
	public ExistFileResponse() {
		super();
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getSourcePath() {
		return sourcePath;
	}

	public void setSourcePath(String sourcePath) {
		this.sourcePath = sourcePath;
	}

	public long getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(long ownerId) {
		this.ownerId = ownerId;
	}

	public boolean isExisted() {
		return existed;
	}

	public void setExisted(boolean existed) {
		this.existed = existed;
	}
	
	

}
