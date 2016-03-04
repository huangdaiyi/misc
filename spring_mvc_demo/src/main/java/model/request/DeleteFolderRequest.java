package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DeleteFolderRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	private boolean extract;

	public DeleteFolderRequest() {
		super();
	}

	public boolean isExtract() {
		return extract;
	}

	public void setExtract(boolean extract) {
		this.extract = extract;
	}

}
