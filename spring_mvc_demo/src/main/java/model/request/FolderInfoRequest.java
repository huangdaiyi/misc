package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class FolderInfoRequest extends RequestBase {

	private static final long serialVersionUID = 1;

	private String path;
	private boolean detail;
	@JsonProperty("display_note")
	private boolean displayNote;
	private String ownerId;
	@JsonProperty("page_number")
	private int pageNumber;
	@JsonProperty("items_per_page")
	private int itemsPerPage;

	public FolderInfoRequest() {
		super();
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public boolean isDetail() {
		return detail;
	}

	public void setDetail(boolean detail) {
		this.detail = detail;
	}

	public boolean isDisplayNote() {
		return displayNote;
	}

	public void setDisplayNote(boolean displayNote) {
		this.displayNote = displayNote;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
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

}
