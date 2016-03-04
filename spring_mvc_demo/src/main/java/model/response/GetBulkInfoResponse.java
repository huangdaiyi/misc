package model.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetBulkInfoResponse implements Serializable {
	private static final long serialVersionUID = 1;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private List<FolderResponse> folders;
	private List<FileResponse> files;

	public GetBulkInfoResponse() {
		super();
	}

	public List<FolderResponse> getFolders() {
		return folders;
	}

	public void setFolders(List<FolderResponse> folders) {
		this.folders = folders;
	}

	public List<FileResponse> getFiles() {
		return files;
	}

	public void setFiles(List<FileResponse> files) {
		this.files = files;
	}
}
