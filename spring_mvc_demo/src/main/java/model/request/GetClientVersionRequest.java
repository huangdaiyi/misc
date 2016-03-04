package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;


@JsonIgnoreProperties(ignoreUnknown = true)

public class GetClientVersionRequest extends RequestBase {
	private static final long serialVersionUID = 1;
	
	private String platform;
	
	public GetClientVersionRequest() {
		super();
	}

	public String getPlatform() {
		return platform;
	}

	public void setPlatform(String platform) {
		this.platform = platform;
	}

	
}
