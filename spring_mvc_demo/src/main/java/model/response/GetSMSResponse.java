package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import constants.HttpStatus;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetSMSResponse implements Serializable {
	
private static final long serialVersionUID = 1;
	
	private String status;
	private String sid;
	@JsonProperty("status_sms")
	private String statusSMS;
	private String message;
	
	public GetSMSResponse() {
		super();
	}

	public int computeSmsStatus() {
		if ("success".equals(status) == false) {
			try {
				return Integer.parseInt(statusSMS);
			} catch (Exception e) {
			}
			return HttpStatus.INTERNAL_SERVER_ERROR.getCode();
		} else if ("undelivered".equals(statusSMS)) {
			return HttpStatus.SMS_UNDELIVERED.getCode();
		}
		return HttpStatus.OK.getCode();
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getSid() {
		return sid;
	}

	public void setSid(String sid) {
		this.sid = sid;
	}

	public String getStatusSMS() {
		return statusSMS;
	}

	public void setStatusSMS(String statusSMS) {
		this.statusSMS = statusSMS;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

}
