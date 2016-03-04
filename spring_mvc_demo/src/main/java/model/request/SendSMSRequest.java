package model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SendSMSRequest implements Serializable {

	private static final long serialVersionUID = 1L;

	private String token;
	private String cellphone;
	@JsonProperty("country_code")
	private String countryCode;
	private String message;

	public SendSMSRequest() {
		super();
	}

	public SendSMSRequest(String token, String cellphone, String countryCode, String message) {
		super();
		this.token = token;
		this.cellphone = cellphone;
		this.countryCode = countryCode;
		this.message = message;
	}

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public String getCellphone() {
		return cellphone;
	}

	public void setCellphone(String cellphone) {
		this.cellphone = cellphone;
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

}
