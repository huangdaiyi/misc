package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DeviceLogout implements Serializable {

	private static final long serialVersionUID = 1L;

	public String uniqid;

	public DeviceLogout() {
		super();
	}

	public String getUniqid() {
		return uniqid;
	}

	public void setUniqid(String uniqid) {
		this.uniqid = uniqid;
	}
}
