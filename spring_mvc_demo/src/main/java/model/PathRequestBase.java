package model;

import utils.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PathRequestBase extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String path;
	@JsonProperty("source_path")
	private String sourcePath = "";
	@JsonProperty("owner_id")
	private String ownerId = "";
	@JsonProperty("shared_root_id")
	private String sharedRootId = "";

	public PathRequestBase() {
		super();
	}

	public void fromPathRequestBase(PathRequestBase pathRequestBase) {
		this.fromRequestBase(pathRequestBase);
		this.setPath(pathRequestBase.getPath());
		this.setSourcePath(pathRequestBase.getSourcePath());
		this.setOwnerId(pathRequestBase.getOwnerId());
		this.setSharedRootId(pathRequestBase.getSharedRootId());
	}

	public String fetchOwnerId() {
		return StringUtils.isNullOrEmpty(ownerId) ? getUserId() : ownerId;
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
}
