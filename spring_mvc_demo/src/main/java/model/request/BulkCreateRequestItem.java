package model.request;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BulkCreateRequestItem extends BulkActionRequestItem {

	private static final long serialVersionUID = 1L;
	
	private String note = "";
	@JsonProperty("icon_block_id")
	private String iconBlockId = "";
	@JsonProperty("icon_text")
	private String iconText = "";
	@JsonProperty("icon_text_color")
	private String iconTextColor = "";
	@JsonProperty("icon_text_style")
	private String iconTextStyle = "";
	
	@JsonProperty("block_id")
	private String blockId;
	private Long size = 0L;
	private Map<String, String> params;
	@JsonProperty("is_encrypted")
	private boolean encrypted = false;
	
	public Map<String, Object> getAttributeMap() {
		Map<String, Object> map = super.getAttributeMap();
		if(this.getNote() != null) map.put("note", this.getNote());
		if(this.getIconBlockId() != null) map.put("icon_block_id", this.getIconBlockId());
		if(this.getIconText() != null) map.put("icon_text", this.getIconText());
		if(this.getIconTextColor() != null) map.put("icon_text_color", this.getIconTextColor());
		if(this.getIconTextStyle() != null) map.put("icon_text_style", this.getIconTextStyle());
		
		if(!this.isFolder()) {
			map.put("block_id", this.getBlockId());
			map.put("size", this.getSize());
			map.put("is_encrypted", this.isEncrypted());
			if (params != null) {
				map.putAll(params);
			}
		}
		
		return map;
	}
	
	public UpdateFolderRequest toCreateFolderRequest() {
		UpdateFolderRequest request = new UpdateFolderRequest();
		request.fromPathRequestBase(this);
		request.setNote(this.getNote());
		request.setIconBlockId(this.getIconBlockId());
		request.setIconText(this.getIconText());
		request.setIconTextColor(this.getIconTextColor());
		request.setIconTextStyle(this.getIconTextStyle());
		return request;
	}
	
	public UpdateFileRequest toCreateFileRequest() {
		UpdateFileRequest request = new UpdateFileRequest();
		request.fromPathRequestBase(this);
		request.setNote(this.getNote());
		request.setIconBlockId(this.getIconBlockId());
		request.setIconText(this.getIconText());
		request.setIconTextColor(this.getIconTextColor());
		request.setIconTextStyle(this.getIconTextStyle());
		request.setBlockId(this.getBlockId());
		request.setSize(this.getSize());
		request.setParams(this.getParams());
		request.setEncrypted(this.isEncrypted());
		return request;
	}

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

	public String getIconBlockId() {
		return iconBlockId;
	}

	public void setIconBlockId(String iconBlockId) {
		this.iconBlockId = iconBlockId;
	}

	public String getIconText() {
		return iconText;
	}

	public void setIconText(String iconText) {
		this.iconText = iconText;
	}

	public String getIconTextColor() {
		return iconTextColor;
	}

	public void setIconTextColor(String iconTextColor) {
		this.iconTextColor = iconTextColor;
	}

	public String getIconTextStyle() {
		return iconTextStyle;
	}

	public void setIconTextStyle(String iconTextStyle) {
		this.iconTextStyle = iconTextStyle;
	}

	public String getBlockId() {
		return blockId;
	}

	public void setBlockId(String blockId) {
		this.blockId = blockId;
	}

	public Long getSize() {
		return size;
	}

	public void setSize(Long size) {
		this.size = size;
	}

	public Map<String, String> getParams() {
		return params;
	}

	public void setParams(Map<String, String> params) {
		this.params = params;
	}

	public boolean isEncrypted() {
		return encrypted;
	}

	public void setEncrypted(boolean encrypted) {
		this.encrypted = encrypted;
	}
	
}
