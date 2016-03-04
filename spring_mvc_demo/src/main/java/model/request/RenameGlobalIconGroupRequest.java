package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class RenameGlobalIconGroupRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String name;
	@JsonProperty("new_name")
	private String newName;
	@JsonProperty("is_folder")
	private boolean folder;

	public RenameGlobalIconGroupRequest() {
		super();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getNewName() {
		return newName;
	}

	public void setNewName(String newName) {
		this.newName = newName;
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}


}
