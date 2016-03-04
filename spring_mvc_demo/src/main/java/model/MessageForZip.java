package model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class MessageForZip {
	
	private String type;
	@JsonProperty("target_path")
	private String targetPath;
	@JsonProperty("target_source_path")
	private String targetSourcePath;
	@JsonProperty("target_owner_id")
	private String targetOwnerId;
	@JsonProperty("target_shared_root_id")
	private String targetSharedRootId;
	@JsonProperty("zip_source")
	private List<ZipSourcePure> zipSource;
	private String user;
	private String password;
	@JsonProperty("x-neweggbox-token")
	private String xNeweggboxToken;
	private String authorization;
	
	public MessageForZip(String type, String targetPath,
			String targetSourcePath, String targetOwnerId, String targetSharedRootId, List<ZipSourcePure> zipSource,
			String user, String password, String xNeweggboxToken,
			String authorization) {
		super();
		this.type = type;
		this.targetPath = targetPath;
		this.targetSourcePath = targetSourcePath;
		this.targetOwnerId = targetOwnerId;
		this.targetSharedRootId = targetSharedRootId;
		this.zipSource = zipSource;
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

	public List<ZipSourcePure> getZipSource() {
		return zipSource;
	}
	public void setZipSource(List<ZipSourcePure> zipSource) {
		this.zipSource = zipSource;
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

