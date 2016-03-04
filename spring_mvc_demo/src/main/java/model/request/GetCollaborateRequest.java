package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetCollaborateRequest extends RequestBase {

	private static final long serialVersionUID = 1L;
	
	@JsonProperty("include_invisible_folder")
	private boolean includeInvisibleFolder = false;
	@JsonProperty("page_number")
	private int pageNumber;
	@JsonProperty("items_per_page")
	private int itemsPerPage;
	private boolean detail;
	@JsonProperty("include_own")
	private boolean includeOwn = true;

	public GetCollaborateRequest() {
		super();
	}
	
	
	public boolean isDetail() {
		return detail;
	}

	public void setDetail(boolean detail) {
		this.detail = detail;
	}

	public boolean isIncludeInvisibleFolder() {
		return includeInvisibleFolder;
	}

	public void setIncludeInvisibleFolder(boolean includeInvisibleFolder) {
		this.includeInvisibleFolder = includeInvisibleFolder;
	}

	public int getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(int pageNumber) {
		this.pageNumber = pageNumber;
	}

	public int getItemsPerPage() {
		return itemsPerPage;
	}

	public void setItemsPerPage(int itemsPerPage) {
		this.itemsPerPage = itemsPerPage;
	}


	public boolean isIncludeOwn() {
		return includeOwn;
	}


	public void setIncludeOwn(boolean includeOwn) {
		this.includeOwn = includeOwn;
	}

	

}
