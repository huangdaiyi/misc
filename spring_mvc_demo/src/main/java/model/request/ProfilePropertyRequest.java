package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProfilePropertyRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;
	
	@JsonProperty("device_unique_id")
	private String deviceUid = "";
	private String name;
	private String value;
	
	public ProfilePropertyRequest() {
		super();
	}
	
	public String getDeviceUid() {
		return this.deviceUid;
	}
	
	public void setDeviceUid(String deviceUid) {
		this.deviceUid = deviceUid;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public String getValue() {
		return value;
	}
	
	public void setValue(String value) {
		this.value = value;
	}

}
