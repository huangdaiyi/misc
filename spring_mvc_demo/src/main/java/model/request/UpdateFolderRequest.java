package model.request;


import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateFolderRequest extends PathRequestBase {
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
	
	public UpdateFolderRequest() {
		
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

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

}
