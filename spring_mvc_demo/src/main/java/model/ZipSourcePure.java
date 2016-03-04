package model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ZipSourcePure {

	private String path;
	@JsonProperty("source_path")
	private String sourcePath;
	@JsonProperty("owner_id")
	private String ownerId;
	@JsonProperty("shared_root_id")
	private String sharedRootId;
	@JsonProperty("is_folder")
	private boolean folder;
	
	public ZipSourcePure(String path, String sourcePath, String ownerId,
			String sharedRootId, boolean folder) {
		super();
		this.path = path;
		this.sourcePath = sourcePath;
		this.ownerId = ownerId;
		this.sharedRootId = sharedRootId;
		this.folder = folder;
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
	
	public String getSharedRootId() {
		return sharedRootId;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}

	public boolean isFolder() {
		return folder;
	}
	public void setFolder(boolean folder) {
		this.folder = folder;
	}
	
}
