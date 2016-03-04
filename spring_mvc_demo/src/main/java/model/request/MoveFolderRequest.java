package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MoveFolderRequest extends PathRequestBase implements Cloneable{

	private static final long serialVersionUID = 1L;

	private String destination;
	private boolean override;
	@JsonProperty("destination_owner_id")
	private String destinationOwnerId;
	@JsonProperty("destination_shared_root_id")
	private String destinationSharedRootId;
	private boolean extract;
	@JsonProperty("force_override")
	private boolean forceOverride;

	@Override
	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}
	
	public MoveFolderRequest() {
		super();
	}

	public String getDestination() {
		return destination;
	}

	public void setDestination(String destination) {
		this.destination = destination;
	}

	public boolean isOverride() {
		return override;
	}

	public void setOverride(boolean override) {
		this.override = override;
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

	public boolean isExtract() {
		return extract;
	}

	public void setExtract(boolean extract) {
		this.extract = extract;
	}

	public boolean isForceOverride() {
		return forceOverride;
	}

	public void setForceOverride(boolean forceOverride) {
		this.forceOverride = forceOverride;
	}
	
}
