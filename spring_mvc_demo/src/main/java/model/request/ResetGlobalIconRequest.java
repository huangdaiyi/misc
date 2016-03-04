package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ResetGlobalIconRequest extends RequestBase {

	private static final long serialVersionUID = 1L;
	@JsonProperty("is_folder")
	private boolean folder;

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

	public ResetGlobalIconRequest() {
		super();
	}

}
