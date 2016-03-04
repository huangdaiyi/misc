package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class GetTextViewSettingRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;
	
	@JsonProperty("viewer_device_unique_id")
	private String viewerDeviceUniqueId = "";

	public String getViewerDeviceUniqueId() {
		return viewerDeviceUniqueId;
	}

	public void setViewerDeviceUniqueId(String viewerDeviceUniqueId) {
		this.viewerDeviceUniqueId = viewerDeviceUniqueId;
	}
}
