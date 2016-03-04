package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import constants.SortType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetFolderRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;

	@JsonProperty("display_note")
	private boolean displayNote;
	@JsonProperty("page_number")
	private int pageNumber;
	@JsonProperty("items_per_page")
	private int itemsPerPage;
	@JsonProperty("force_sortby_type")
	private SortType forceSortByType;
	private String password;
	@JsonProperty("viewer_device_unique_id")
	private String viewerDeviceUniqueId = "";

	public GetFolderRequest() {
		super();
	}

	public boolean isDisplayNote() {
		return displayNote;
	}

	public void setDisplayNote(boolean displayNote) {
		this.displayNote = displayNote;
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

	public SortType getForceSortByType() {
		return forceSortByType;
	}

	public void setForceSortByType(SortType forceSortByType) {
		this.forceSortByType = forceSortByType;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getViewerDeviceUniqueId() {
		return viewerDeviceUniqueId;
	}

	public void setViewerDeviceUniqueId(String viewerDeviceUniqueId) {
		this.viewerDeviceUniqueId = viewerDeviceUniqueId;
	}
}
