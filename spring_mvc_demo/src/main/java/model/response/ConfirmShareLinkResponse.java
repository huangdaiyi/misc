package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ConfirmShareLinkResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private boolean result;

	public ConfirmShareLinkResponse() {
		super();
	}
	
	public ConfirmShareLinkResponse(boolean result){
		this.result = result;
	}

	public boolean isResult() {
		return result;
	}

	public void setResult(boolean result) {
		this.result = result;
	}

}
