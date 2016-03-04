package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MoveFileRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;

	private String destination;
	@JsonProperty("destination_owner_id")
	private String destinationOwnerId;
	@JsonProperty("destination_shared_root_id")
	private String destinationSharedRootId = "";
	private boolean override;
	@JsonProperty("force_override")
	private boolean forceOverride;
	
	public MoveFileRequest() {
		super();
	}

	public String getDestination() {
		return destination;
	}

	public void setDestination(String destination) {
		this.destination = destination;
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


	public boolean isOverride() {
		return override;
	}

	public void setOverride(boolean override) {
		this.override = override;
	}

	public boolean isForceOverride() {
		return forceOverride;
	}

	public void setForceOverride(boolean forceOverride) {
		this.forceOverride = forceOverride;
	}

	
}
