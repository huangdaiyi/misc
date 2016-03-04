package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CancelCollaborateRequest extends PathRequestBase {
	
	private static final long serialVersionUID = 1L;
	
	@JsonProperty("return_to_creator")
	private boolean returnToCreator = true;

	public CancelCollaborateRequest() {
		super();
	}

	public boolean isReturnToCreator() {
		return returnToCreator;
	}

	public void setReturnToCreator(boolean returnToCreator) {
		this.returnToCreator = returnToCreator;
	}	
}