package model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class MessageForStreaming {
	
	@JsonProperty("download_url")
	private String downloadUrl;
	@JsonProperty("base_url")
	private String baseUrl;
	private String extension;
	private String path;
	@JsonProperty("source_path")
	private String sourcePath;
	@JsonProperty("owner_id")
	private String ownerId;
	@JsonProperty("neweggbox_sso_token")
	private String neweggboxSsoToken;
	private String authorization;
	
	public MessageForStreaming(String downloadUrl, String baseUrl,
			String extension, String path, String sourcePath, String ownerId,
			String neweggboxSsoToken, String authorization) {
		super();
		this.downloadUrl = downloadUrl;
		this.baseUrl = baseUrl;
		this.extension = extension;
		this.path = path;
		this.sourcePath = sourcePath;
		this.ownerId = ownerId;
		this.neweggboxSsoToken = neweggboxSsoToken;
		this.authorization = authorization;
	}
	
	public String getDownloadUrl() {
		return downloadUrl;
	}
	public void setDownloadUrl(String downloadUrl) {
		this.downloadUrl = downloadUrl;
	}
	public String getBaseUrl() {
		return baseUrl;
	}
	public void setBaseUrl(String baseUrl) {
		this.baseUrl = baseUrl;
	}
	public String getExtension() {
		return extension;
	}
	public void setExtension(String extension) {
		this.extension = extension;
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
	public String getNeweggboxSsoToken() {
		return neweggboxSsoToken;
	}
	public void setNeweggboxSsoToken(String neweggboxSsoToken) {
		this.neweggboxSsoToken = neweggboxSsoToken;
	}
	public String getAuthorization() {
		return authorization;
	}
	public void setAuthorization(String authorization) {
		this.authorization = authorization;
	}
	
}


