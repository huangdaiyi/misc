package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AddGlobalIconRequest extends RequestBase {
	private static final long serialVersionUID = 1;
	@JsonProperty("block_id")
	private String blockId;
	@JsonProperty("group_name")
	private String groupName;
	@JsonProperty("is_folder")
	private boolean folder;
	
	public String getBlockId() {
		return blockId;
	}
	public void setBlockId(String blockId) {
		this.blockId = blockId;
	}
	public String getGroupName() {
		return groupName;
	}
	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}
	public boolean isFolder() {
		return folder;
	}
	public void setFolder(boolean folder) {
		this.folder = folder;
	}

}