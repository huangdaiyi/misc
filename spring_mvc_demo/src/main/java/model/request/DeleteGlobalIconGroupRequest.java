package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DeleteGlobalIconGroupRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String name;
	@JsonProperty("is_folder")
	private Boolean folder;

	public DeleteGlobalIconGroupRequest() {
		super();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Boolean getFolder() {
		return folder;
	}

	public void setFolder(Boolean folder) {
		this.folder = folder;
	}

	
}
