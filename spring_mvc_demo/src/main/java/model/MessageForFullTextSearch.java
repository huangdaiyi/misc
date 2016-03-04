package model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class MessageForFullTextSearch {
	
	@JsonProperty("type")
	private String type;
	@JsonProperty("owner")
	private String ownerId;
	@JsonProperty("block")
	private String blockId;
	@JsonProperty("file_url")
	private String fileUrl;
	
	public MessageForFullTextSearch(String type, String ownerId, String blockId, String fileUrl) {
		super();
		this.type = type;
		this.ownerId = ownerId;
		this.blockId = blockId;
		this.fileUrl = fileUrl; 
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}

	public String getBlockId() {
		return blockId;
	}

	public void setBlockId(String blockId) {
		this.blockId = blockId;
	}

	public String getFileUrl() {
		return fileUrl;
	}

	public void setFileUrl(String fileUrl) {
		this.fileUrl = fileUrl;
	}

}
