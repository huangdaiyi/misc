package model.request;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SyncRelationRequest extends SyncRelationPathRequest {

	private static final long serialVersionUID = 1L;
	
	// target

	@JsonProperty("destination_path")
	private String destinationPath = ""; // path or source path
	@JsonProperty("destination_owner_id")
	private String destinationOwnerId = "";
	@JsonProperty("destination_shared_root_id")
	private String destinationSharedRootId = "";
	
	// other
	
	private String action = "";
	private boolean enable = true;
	
	public SyncRelationPathRequest parseDestinationToSyncRelationPathRequest() {
		SyncRelationPathRequest request = new SyncRelationPathRequest();
		request.fromRequestBase(this);
		request.setPath(destinationPath);
		request.setOwnerId(destinationOwnerId);
		request.setSharedRootId(destinationSharedRootId);
		return request;
	}
	
	// getters & setters
	
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
