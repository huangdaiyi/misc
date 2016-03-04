package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class LinkFoldersRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String path;

	public LinkFoldersRequest() {
		super();
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}
}
