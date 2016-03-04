package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SyncRelationResponse implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private String path = ""; // path or source path
	@JsonProperty("owner_id")
	private String ownerId = "";
	@JsonProperty("shared_root_id")
	private String sharedRootId = "";
	
	@JsonProperty("destination_path")
	private String destinationPath = ""; // path or source path
	@JsonProperty("destination_owner_id")
	private String destinationOwnerId = "";
	@JsonProperty("destination_shared_root_id")
	private String destinationSharedRootId = "";
	
	private String action = "";
	private boolean enable = true;

	public SyncRelationResponse() {
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

	public String getSharedRootId() {
		return sharedRootId;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}

	public String getDestinationPath() {
		return destinationPath;
	}

	public void setDestinationPath(String destinationPath) {
		this.destinationPath = destinationPath;
	}

	public String getDestinationOwnerId() {
		return destinationOwnerId;
	}

	public void setDestinationOwnerId(String destinationOwnerId) {
		this.destinationOwnerId = destinationOwnerId;
	}

	public String getDestinationSharedRootId() {
		return destinationSharedRootId;
	}

	public void setDestinationSharedRootId(String destinationSharedRootId) {
		this.destinationSharedRootId = destinationSharedRootId;
	}

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = action;
	}

	public boolean isEnable() {
		return enable;
	}

	public void setEnable(boolean enable) {
		this.enable = enable;
	}

}
