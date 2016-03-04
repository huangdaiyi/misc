package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetSearchResultRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;

	
	private String query;
	@JsonProperty("page_number")
	private int pageNumber;
	@JsonProperty("items_per_page")
	private int itemsPerPage;
	@JsonProperty("include_backup")
	private boolean includeBackup = false;

	public String getQuery() {
		return query;
	}

	public void setQuery(String query) {
		this.query = query;
	}

	public GetSearchResultRequest() {
		super();
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

	public boolean isIncludeBackup() {
		return includeBackup;
	}

	public void setIncludeBackup(boolean includeBackup) {
		this.includeBackup = includeBackup;
	}
}
