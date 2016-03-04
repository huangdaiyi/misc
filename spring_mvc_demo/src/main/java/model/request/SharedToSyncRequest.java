package model.request;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SharedToSyncRequest extends SyncRelationRequest {

	private static final long serialVersionUID = 1L;
	
	@JsonProperty("return_to_creator")
	private boolean returnToCreator = true;

	public boolean isReturnToCreator() {
		return returnToCreator;
	}

	public void setReturnToCreator(boolean returnToCreator) {
		this.returnToCreator = returnToCreator;
	}

}
