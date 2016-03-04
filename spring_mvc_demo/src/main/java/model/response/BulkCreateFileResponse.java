package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkCreateFileResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private String path;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("owner_id")
	private String ownerId;
	@JsonProperty("last_edit_time")
	private long modifiedAt;
	private String type;
	private boolean succeed;

	public BulkCreateFileResponse() {
		super();
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}

	public long getModifiedAt() {
		return modifiedAt;
	}

	public void setModifiedAt(long modifiedAt) {
		this.modifiedAt = modifiedAt;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public boolean isSucceed() {
		return succeed;
	}

	public void setSucceed(boolean succeed) {
		this.succeed = succeed;
	}

}
