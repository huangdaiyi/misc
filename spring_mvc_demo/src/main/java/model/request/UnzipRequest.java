package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UnzipRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("target_path")
	private String targetPath;
	@JsonProperty("target_owner_id")
	private String targetOwnerId = "";
	@JsonProperty("target_shared_root_id")
	private String targetSharedRootId = "";
	private String password = "";

	public UnzipRequest() {
		super();
	}

	public String getTargetPath() {
		return targetPath;
	}

	public void setTargetPath(String targetPath) {
		this.targetPath = targetPath;
	}

	public String getTargetOwnerId() {
		return targetOwnerId;
	}

	public void setTargetOwnerId(String targetOwnerId) {
		this.targetOwnerId = targetOwnerId;
	}

	public String getTargetSharedRootId() {
		return targetSharedRootId;
	}

	public void setTargetSharedRootId(String targetSharedRootId) {
		this.targetSharedRootId = targetSharedRootId;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}
}
