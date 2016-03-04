package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)

public class ClientVersionResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String platform;
	private String version;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("update_type")
	private String updateType;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("download_url")
	private String downloadUrl;
	@JsonProperty("stable_api_version")
	private String stableApiVersion;

	public ClientVersionResponse() {
		super();
	}

	public String getPlatform() {
		return platform;
	}

	public void setPlatform(String platform) {
		this.platform = platform;
	}

	public String getUpdateType() {
		return updateType;
	}

	public void setUpdateType(String updateType) {
		this.updateType = updateType;
	}

	public String getDownloadUrl() {
		return downloadUrl;
	}

	public void setDownloadUrl(String downloadUrl) {
		this.downloadUrl = downloadUrl;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public String getStableApiVersion() {
		return stableApiVersion;
	}

	public void setStableApiVersion(String stableApiVersion) {
		this.stableApiVersion = stableApiVersion;
	}

}
