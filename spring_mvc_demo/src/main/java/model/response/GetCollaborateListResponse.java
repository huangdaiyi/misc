package model.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import constants.SortType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetCollaborateListResponse implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@JsonProperty("total_pages")
	private int totalPages;
	@JsonProperty("sortby_type")
	private SortType sortByType;
	private List<GetCollaborateInfoResponse> folders;
	
	public GetCollaborateListResponse() {
		super();
	}

	public int getTotalPages() {
		return totalPages;
	}

	public void setTotalPages(int totalPages) {
		this.totalPages = totalPages;
	}

	public SortType getSortByType() {
		return sortByType;
	}

	public void setSortByType(SortType sortByType) {
		this.sortByType = sortByType;
	}

	public List<GetCollaborateInfoResponse> getFolders() {
		return folders;
	}

	public void setFolders(List<GetCollaborateInfoResponse> folders) {
		this.folders = folders;
	}

}
