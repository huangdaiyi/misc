package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class RenameFolderRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("new_name")
	private String newName;

	public RenameFolderRequest() {
		super();
	}

	public String getNewName() {
		return newName;
	}

	public void setNewName(String newName) {
		this.newName = newName;
	}

}
