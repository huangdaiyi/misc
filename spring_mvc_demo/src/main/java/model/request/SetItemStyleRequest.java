package model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import model.PathRequestBase;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SetItemStyleRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;
	
	@JsonProperty("is_folder")
	private Boolean folder;
	@JsonProperty("item_text_color")
	private String itemTextColor;
	@JsonProperty("item_bg_color")
	private String itemBgColor;
	@JsonProperty("item_text_bold")
	private Boolean itemTextBold;
	@JsonProperty("item_text_italic")
	private Boolean itemTextItalic;
	@JsonProperty("item_text_underline")
	private Boolean itemTextUnderline;
	
	public Boolean isFolder() {
		return folder;
	}
	public void setFolder(Boolean folder) {
		this.folder = folder;
	}
	public String getItemTextColor() {
		return itemTextColor;
	}
	public void setItemTextColor(String itemTextColor) {
		this.itemTextColor = itemTextColor;
	}
	public String getItemBgColor() {
		return itemBgColor;
	}
	public void setItemBgColor(String itemBgColor) {
		this.itemBgColor = itemBgColor;
	}
	public Boolean isItemTextBold() {
		return itemTextBold;
	}
	public void setItemTextBold(Boolean itemTextBold) {
		this.itemTextBold = itemTextBold;
	}
	public Boolean isItemTextItalic() {
		return itemTextItalic;
	}
	public void setItemTextItalic(Boolean itemTextItalic) {
		this.itemTextItalic = itemTextItalic;
	}
	public Boolean isItemTextUnderline() {
		return itemTextUnderline;
	}
	public void setItemTextUnderline(Boolean itemTextUnderline) {
		this.itemTextUnderline = itemTextUnderline;
	}
}
