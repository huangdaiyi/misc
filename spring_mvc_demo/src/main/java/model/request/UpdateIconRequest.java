package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateIconRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("block_id")
	private String blockId;
	@JsonProperty("icon_text")
	private String iconText;
	@JsonProperty("icon_text_color")
	private String iconTextColor;
	@JsonProperty("icon_text_style")
	private String iconTextStyle;

	public String getIconText() {
		return iconText == null?"":iconText;
	}

	public void setIconText(String iconText) {
		this.iconText = iconText;
	}

	public String getIconTextColor() {

		return iconTextColor == null ?"":iconTextColor;
	}

	public void setIconTextColor(String iconTextColor) {
		this.iconTextColor = iconTextColor;
	}

	public String getIconTextStyle() {
		return iconTextStyle==null?"":iconTextStyle;
	}

	public void setIconTextStyle(String iconTextStyle) {
		this.iconTextStyle = iconTextStyle;
	}

	public UpdateIconRequest() {
		super();
	}

	public String getBlockId() {
		return blockId;
	}

	public void setBlockId(String blockId) {
		this.blockId = blockId;
	}

}
