package model.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetBulkFileInfoResponse implements Serializable {
	private static final long serialVersionUID = 1;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private List<FolderResponse> folders;
	private List<FileResponse> files;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("total_pages")
	private Integer totalPages;

	public GetBulkFileInfoResponse() {
		super();
	}
	
	public GetBulkFileInfoResponse(int totalPages, List<FolderResponse> folders, List<FileResponse> files) {
		super();
		this.totalPages = totalPages;
		this.folders = folders;
		this.files = files;
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

	public Integer getTotalPages() {
		return totalPages;
	}

	public void setTotalPages(Integer totalPages) {
		this.totalPages = totalPages;
	}

	
}
