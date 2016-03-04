package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CellPhone implements Serializable {

	private static final long serialVersionUID = 1L;

	private String cellphone;
	@JsonProperty("country_code")
	private String countryCode;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("user_id")
	private String userId;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("sms_status_code")
	private Integer smsStatusCode = null;

	public CellPhone() {
		super();
	}

	public boolean isCellphoneEquals(CellPhone compareTo) {
		if (compareTo == null) {
			return false;
		} else if (cellphone == null || countryCode == null) {
			return false;
		}
		return (cellphone.equals(compareTo.getCellphone()) && countryCode
				.equals(compareTo.getCountryCode()));
	}

	public String getCellphone() {
		return cellphone;
	}

	public void setCellphone(String cellphone) {
		this.cellphone = cellphone.trim();
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode.trim();
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	@Override
	public String toString() {
		return cellphone + "," + countryCode;
	}

	public Integer getSmsStatusCode() {
		return smsStatusCode;
	}

	public void setSmsStatusCode(Integer smsStatusCode) {
		this.smsStatusCode = smsStatusCode;
	}

}
