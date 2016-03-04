package model.request;

import model.PathRequestBase;
import model.TextViewSetting;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UpdateTextViewSettingRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;
	
	@JsonProperty("viewer_device_unique_id")
	private String viewerDeviceUniqueId = "";
	@JsonProperty("font_color")
	private String fontColor = "";
	@JsonProperty("bg_color")
	private String bgColor = "";
	private Double zoom = 1.0;
	private Boolean fullscreen = false;
	@JsonProperty("font_name")
	private String fontName = "";
	@JsonProperty("font_style")
	private String fontStyle = "";
	
	public TextViewSetting toTextViewSetting() {
		TextViewSetting textViewSetting = new TextViewSetting();
		textViewSetting.setViewerId(getUserId());
		textViewSetting.setViewerDeviceUniqueId(viewerDeviceUniqueId);
		textViewSetting.setFontColor(fontColor);
		textViewSetting.setBgColor(bgColor);
		textViewSetting.setZoom(zoom);
		textViewSetting.setFullscreen(fullscreen);
		textViewSetting.setFontName(fontName);
		textViewSetting.setFontStyle(fontStyle);
		return textViewSetting;
	}

	public String getViewerDeviceUniqueId() {
		return viewerDeviceUniqueId;
	}

	public void setViewerDeviceUniqueId(String viewerDeviceUniqueId) {
		this.viewerDeviceUniqueId = viewerDeviceUniqueId;
	}

	public String getFontColor() {
		return fontColor;
	}

	public void setFontColor(String fontColor) {
		this.fontColor = fontColor;
	}

	public String getBgColor() {
		return bgColor;
	}

	public void setBgColor(String bgColor) {
		this.bgColor = bgColor;
	}

	public Double getZoom() {
		return zoom;
	}

	public void setZoom(Double zoom) {
		this.zoom = zoom;
	}

	public Boolean getFullscreen() {
		return fullscreen;
	}

	public void setFullscreen(Boolean fullscreen) {
		this.fullscreen = fullscreen;
	}

	public String getFontName() {
		return fontName;
	}

	public void setFontName(String fontName) {
		this.fontName = fontName;
	}

	public String getFontStyle() {
		return fontStyle;
	}

	public void setFontStyle(String fontStyle) {
		this.fontStyle = fontStyle;
	}

}
