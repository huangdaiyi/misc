package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class EmailResponse implements Serializable {

	private static final long serialVersionUID = 1;

	@JsonProperty("IsSendSuccess")
	private boolean sendSuccess;

	public EmailResponse() {
		super();
	}

	public boolean isSendSuccess() {
		return sendSuccess;
	}

	public void setSendSuccess(boolean sendSuccess) {
		this.sendSuccess = sendSuccess;
	}

}
