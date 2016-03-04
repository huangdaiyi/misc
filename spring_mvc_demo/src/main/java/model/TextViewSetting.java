package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class TextViewSetting implements Serializable {
	
	private static final long serialVersionUID = 1;
	
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("metadata_index_id")
	private String metadataIndexId = "";
	@JsonProperty("viewer_id")
	private String viewerId = "";
	@JsonProperty("viewer_device_unique_id")
	private String viewerDeviceUniqueId = "";
	@JsonProperty("font_color")
	private String fontColor = "";
	@JsonProperty("bg_color")
	private String bgColor = "";
	private Double zoom = 0.0;
	private Boolean fullscreen = false;
	@JsonProperty("font_name")
	private String fontName = "";
	@JsonProperty("font_style")
	private String fontStyle = "";
	
	public String getMetadataIndexId() {
		return metadataIndexId;
	}
	
	public void setMetadataIndexId(String metadataIndexId) {
		this.metadataIndexId = metadataIndexId;
	}
	
	public String getViewerId() {
		return viewerId;
	}
	
	public void setViewerId(String viewerId) {
		this.viewerId = viewerId;
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
