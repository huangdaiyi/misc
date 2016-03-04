package model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class MessageForUnzip {
	
	private String type;
	private String path;
	@JsonProperty("source_path")
	private String sourcePath;
	@JsonProperty("owner_id")
	private String ownerId;
	@JsonProperty("shared_root_id")
	private String sharedRootId;
	@JsonProperty("target_path")
	private String targetPath;
	@JsonProperty("target_source_path")
	private String targetSourcePath;
	@JsonProperty("target_owner_id")
	private String targetOwnerId;
	@JsonProperty("target_shared_root_id")
	private String targetSharedRootId;
	private String user;
	private String password;
	@JsonProperty("x-neweggbox-token")
	private String xNeweggboxToken;
	private String authorization;
	
	public MessageForUnzip(String type, String path, String sourcePath, String ownerId, String sharedRootId,
			String targetPath, String targetSourcePath, String targetOwnerId, String targetSharedRootId, String user,
			String password, String xNeweggboxToken, String authorization) {
		super();
		this.type = type;
		this.path = path;
		this.sourcePath = sourcePath;
		this.ownerId = ownerId;
		this.sharedRootId = sharedRootId;
		this.targetPath = targetPath;
		this.targetSourcePath = targetSourcePath;
		this.targetOwnerId = targetOwnerId;
		this.targetSharedRootId = targetSharedRootId;
		this.user = user;
		this.password = password;
		this.xNeweggboxToken = xNeweggboxToken;
		this.authorization = authorization;
	}
	
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
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
	public String getTargetPath() {
		return targetPath;
	}
	public void setTargetPath(String targetPath) {
		this.targetPath = targetPath;
	}
	public String getTargetSourcePath() {
		return targetSourcePath;
	}
	public void setTargetSourcePath(String targetSourcePath) {
		this.targetSourcePath = targetSourcePath;
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
	public String getUser() {
		return user;
	}
	public void setUser(String user) {
		this.user = user;
	}
	public String getPassword() {
		return password;
	}
	public void setPassword(String password) {
		this.password = password;
	}
	public String getxNeweggboxToken() {
		return xNeweggboxToken;
	}
	public void setxNeweggboxToken(String xNeweggboxToken) {
		this.xNeweggboxToken = xNeweggboxToken;
	}
	public String getAuthorization() {
		return authorization;
	}
	public void setAuthorization(String authorization) {
		this.authorization = authorization;
	}

}


