package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetDeviceRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("unique_id")
	private String deviceUid;

	public GetDeviceRequest() {
		super();
	}

	public String getDeviceUid() {
		return deviceUid;
	}

	public void setDeviceUid(String deviceUid) {
		this.deviceUid = deviceUid;
	}

}
