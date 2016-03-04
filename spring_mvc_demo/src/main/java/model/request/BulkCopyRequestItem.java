package model.request;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BulkCopyRequestItem extends BulkActionRequestItem {

	private static final long serialVersionUID = 1L;
	
	private String destination;
	private boolean override;
	@JsonProperty("destination_owner_id")
	private String destinationOwnerId;
	@JsonProperty("destination_shared_root_id")
	private String destinationSharedRootId;
	private boolean extract;
	@JsonProperty("force_override")
	private boolean forceOverride = false;
	
	public Map<String, Object> getAttributeMap() {
		Map<String, Object> map = super.getAttributeMap();
		map.put("override", this.isOverride());
		if(this.isFolder()) map.put("extract", this.isExtract());
		if(this.getDestination() != null) map.put("destination", this.getDestination());
		if(this.getDestinationOwnerId() != null) map.put("destination_owner_id", this.getDestinationOwnerId());
		if(this.getDestinationSharedRootId() != null) map.put("destination_shared_root_id", this.getDestinationSharedRootId());
		return map;
	}
	
	public CopyFolderRequest toCopyFolderRequest() {
		CopyFolderRequest request = new CopyFolderRequest();
		request.fromPathRequestBase(this);
		request.setDestination(this.getDestination());
		request.setOverride(this.isOverride());
		request.setDestinationOwnerId(this.getDestinationOwnerId());
		request.setDestinationSharedRootId(this.getDestinationSharedRootId());
		request.setExtract(this.isExtract());
		request.setForceOverride(this.isForceOverride());
		return request;
	}
	
	public CopyFileRequest toCopyFileRequest() {
		CopyFileRequest request = new CopyFileRequest();
		request.fromPathRequestBase(this);
		request.setDestination(this.getDestination());
		request.setOverride(this.isOverride());
		request.setDestinationOwnerId(this.getDestinationOwnerId());
		request.setDestinationSharedRootId(this.getDestinationSharedRootId());
		request.setForceOverride(this.isForceOverride());
		return request;
	}

	public boolean isExtract() {
		return extract;
	}

	public void setExtract(boolean extract) {
		this.extract = extract;
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

	public boolean isForceOverride() {
		return forceOverride;
	}

	public void setForceOverride(boolean forceOverride) {
		this.forceOverride = forceOverride;
	}
	
}
