package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SSOUserInfoRequest extends RequestBase {

	private static final long serialVersionUID = 1;

	@JsonProperty("token")
	private String token;

	public SSOUserInfoRequest() {
		super();
	}

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

}
