package model.request;

import java.util.List;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class DeleteGlobalIconRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("block_ids")
	private List<String> blockIds;
	@JsonProperty("is_folder")
	private boolean folder;
	@JsonProperty("group_name")
	private String groupName;

	public DeleteGlobalIconRequest() {
		super();
	}
	
	public List<String> getBlockIds() {
		return blockIds;
	}

	public void setBlockIds(List<String> blockIds) {
		this.blockIds = blockIds;
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}
	
	
	
}