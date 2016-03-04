package model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class MessageForDecrypt {

	private String type = "decrypt";
	private String path;
	@JsonProperty("source_path")
	private String sourcePath;
	@JsonProperty("owner_id")
	private String ownerId;
	@JsonProperty("shared_root_id")
	private String sharedRootId;
	private String password;
	private String user;
	@JsonProperty("x-neweggbox-token")
	private String token;
	private String authorization;
	
	public MessageForDecrypt(String path, String sourcePath, String ownerId,
			String sharedRootId, String password, String user, String token, String authorization) {
		this.path = path;
		this.sourcePath = sourcePath;
		this.ownerId = ownerId;
		this.sharedRootId = sharedRootId;
		this.password = password;
		this.user = user;
		this.token = token;
		this.authorization = authorization;
	}

	public String getType() {
		return type;
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
	
	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getUser() {
		return user;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public String getAuthorization() {
		return authorization;
	}

	public void setAuthorization(String authorization) {
		this.authorization = authorization;
	}
	
}
