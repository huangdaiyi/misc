package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)

public class ExistFile implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private String path;
	@JsonProperty("source_path")
	private String sourcePath;
	@JsonProperty("owner_id")
	private String ownerId;
	@JsonProperty("shared_root_id")
	private String sharedRootId;
	private boolean existed;
	
	public ExistFile() {
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

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}

	public boolean isExisted() {
		return existed;
	}

	public void setExisted(boolean existed) {
		this.existed = existed;
	}

	public String getSharedRootId() {
		return sharedRootId;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}
	
	
}
