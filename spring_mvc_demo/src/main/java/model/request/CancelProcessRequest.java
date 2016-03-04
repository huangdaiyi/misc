package model.request;

import com.fasterxml.jackson.annotation.JsonProperty;

import model.PathRequestBase;

public class CancelProcessRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;
	
	@JsonProperty("is_folder")
	private Boolean folder;

	public Boolean getFolder() {
		return folder;
	}

	public void setFolder(Boolean folder) {
		this.folder = folder;
	}
	
}
