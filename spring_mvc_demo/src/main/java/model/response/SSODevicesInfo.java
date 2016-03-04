package model.response;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SSODevicesInfo implements Serializable {

	private static final long serialVersionUID = 1L;
	private String status;
	private List<Map<String, String>> devices;
	public String getStatus() {
		return status;
	}
	public void setStatus(String status) {
		this.status = status;
	}
	public List<Map<String, String>> getDevices() {
		return devices;
	}
	public void setDevices(List<Map<String, String>> devices) {
		this.devices = devices;
	}

}
