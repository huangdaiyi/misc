package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DeleteFolderBackupRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String path;
	@JsonProperty("recursive_clean")
	private boolean recursiveClean;

	public DeleteFolderBackupRequest() {
		super();
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public boolean isRecursiveClean() {
		return recursiveClean;
	}

	public void setRecursiveClean(boolean recursiveClean) {
		this.recursiveClean = recursiveClean;
	}

}
