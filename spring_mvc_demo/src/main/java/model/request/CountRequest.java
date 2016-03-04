package model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import model.PathRequestBase;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CountRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	private String ownerId;
	private boolean backUp;

	public boolean isBackUp() {
		return backUp;
	}

	public void setBackUp(boolean backUp) {
		this.backUp = backUp;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}
}
