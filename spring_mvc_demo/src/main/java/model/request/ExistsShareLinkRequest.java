package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ExistsShareLinkRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("is_folder")
	private boolean folder;

	public ExistsShareLinkRequest() {
		super();
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

}
