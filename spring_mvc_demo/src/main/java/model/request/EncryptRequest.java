package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class EncryptRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;

	private String password;
	@JsonProperty("send_notification")
	private boolean sendNotification = false;

	public EncryptRequest() {
		super();
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public boolean isSendNotification() {
		return sendNotification;
	}

	public void setSendNotification(boolean sendNotification) {
		this.sendNotification = sendNotification;
	}
	
}