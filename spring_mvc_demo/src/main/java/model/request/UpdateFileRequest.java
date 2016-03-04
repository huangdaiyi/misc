package model.request;

import java.util.Map;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateFileRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;
	
	private String note = "";
	@JsonProperty("block_id")
	private String blockId;
	private Long size = 0L;
	private Map<String, String> params;
	@JsonProperty("icon_block_id")
	private String iconBlockId = "";
	@JsonProperty("icon_text")
	private String iconText = "";
	@JsonProperty("icon_text_color")
	private String iconTextColor = "";
	@JsonProperty("icon_text_style")
	private String iconTextStyle = "";
	@JsonProperty("is_encrypted")
	private boolean encrypted = false;

	public UpdateFileRequest() {
		super();
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

	public String getIconBlockId() {
		return iconBlockId;
	}

	public void setIconBlockId(String iconBlockId) {
		this.iconBlockId = iconBlockId;
	}

	public boolean isEncrypted() {
		return encrypted;
	}

	public void setEncrypted(boolean encrypted) {
		this.encrypted = encrypted;
	}

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

}
