package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetFileProcessingStatusRequest extends PathRequestBase {

	private static final long serialVersionUID = 1;

	@JsonProperty("is_folder")
	private boolean folder;

	public GetFileProcessingStatusRequest() {
		super();
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

}
