package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetUserAppRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("device_unique_id")
	private String deviceUniqueId = "";

	public GetUserAppRequest() {
		super();
	}

	public String getDeviceUniqueId() {
		return deviceUniqueId;
	}

	public void setDeviceUniqueId(String deviceUniqueId) {
		this.deviceUniqueId = deviceUniqueId;
	}

}
