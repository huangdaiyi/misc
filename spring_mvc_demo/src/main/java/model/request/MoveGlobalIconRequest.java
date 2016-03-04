package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MoveGlobalIconRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("group_name")
	private String groupName;
	private String destination;
	@JsonProperty("block_id")
	private String blockId;
	@JsonProperty("is_folder")
	private boolean folder;

	public MoveGlobalIconRequest() {
		super();
	}

	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	public String getDestination() {
		return destination;
	}

	public void setDestination(String destination) {
		this.destination = destination;
	}

	public String getBlockId() {
		return blockId;
	}

	public void setBlockId(String blockId) {
		this.blockId = blockId;
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}


}
