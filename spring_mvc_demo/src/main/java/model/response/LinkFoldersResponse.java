package model.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class LinkFoldersResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private List<LinkFolderResponse> folders;

	public LinkFoldersResponse() {
		super();
	}

	public List<LinkFolderResponse> getFolders() {
		return folders;
	}

	public void setFolders(List<LinkFolderResponse> folders) {
		this.folders = folders;
	}
}
