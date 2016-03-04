package model.response;

import java.io.Serializable;
import java.util.List;

public class GetAllDeviceResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private List<GetDeviceResponse> devices;

	public GetAllDeviceResponse() {
		super();
	}

	public List<GetDeviceResponse> getDevices() {
		return devices;
	}

	public void setDevices(List<GetDeviceResponse> devices) {
		this.devices = devices;
	}

}
