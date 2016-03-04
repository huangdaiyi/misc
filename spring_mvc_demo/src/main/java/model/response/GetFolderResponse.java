package model.response;

import java.io.Serializable;
import java.util.List;

import model.ItemStyle;
import model.PageProfileProperty;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import constants.GridType;
import constants.SortType;
import constants.ViewType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetFolderResponse implements Serializable {
	private static final long serialVersionUID = 1;

	@JsonProperty("total_pages")
	private int totalPages;
	@JsonProperty("sortby_type")
	private SortType sortByType;
	@JsonProperty("viewby_type")
	private ViewType viewByType;
	@JsonProperty("gridby_type")
	private GridType gridByType;
	@JsonProperty("has_image")
	private boolean hasImage = false;
	private List<FolderResponse> folders;
	private List<FileResponse> files;
	@JsonProperty("item_text_color")
	private String itemTextColor;
	@JsonProperty("item_bg_color")
	private String itemBgColor;
	@JsonProperty("item_text_bold")
	private Boolean itemTextBold;
	@JsonProperty("item_text_italic")
	private Boolean itemTextItalic;
	@JsonProperty("item_text_underline")
	private Boolean itemTextUnderline;

	public GetFolderResponse() {
		super();
	}

	public GetFolderResponse(int totalPages, PageProfileProperty pageProfileProperty, boolean hasImage, 
			ItemStyle itemStyle, List<FolderResponse> folders, List<FileResponse> files) {
		super();
		this.totalPages = totalPages;
		this.sortByType = pageProfileProperty.getSortByType();
		this.viewByType = pageProfileProperty.getViewByType();
		this.gridByType = pageProfileProperty.getGridByType();
		this.hasImage = hasImage;
		this.folders = folders;
		this.files = files;
		if (itemStyle != null) {
			this.itemTextColor = itemStyle.getItemTextColor();
			this.itemBgColor = itemStyle.getItemBgColor();
			this.itemTextBold = itemStyle.isItemTextBold();
			this.itemTextItalic = itemStyle.isItemTextItalic();
			this.itemTextUnderline = itemStyle.isItemTextUnderline();
		}
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

	public boolean isHasImage() {
		return hasImage;
	}

	public void setHasImage(boolean hasImage) {
		this.hasImage = hasImage;
	}

	public ViewType getViewByType() {
		return viewByType;
	}

	public void setViewByType(ViewType viewByType) {
		this.viewByType = viewByType;
	}

	public GridType getGridByType() {
		return gridByType;
	}

	public void setGridByType(GridType gridByType) {
		this.gridByType = gridByType;
	}

	public String getItemTextColor() {
		return itemTextColor;
	}

	public void setItemTextColor(String itemTextColor) {
		this.itemTextColor = itemTextColor;
	}

	public String getItemBgColor() {
		return itemBgColor;
	}

	public void setItemBgColor(String itemBgColor) {
		this.itemBgColor = itemBgColor;
	}

	public Boolean isItemTextBold() {
		return itemTextBold;
	}

	public void setItemTextBold(Boolean itemTextBold) {
		this.itemTextBold = itemTextBold;
	}

	public Boolean isItemTextItalic() {
		return itemTextItalic;
	}

	public void setItemTextItalic(Boolean itemTextItalic) {
		this.itemTextItalic = itemTextItalic;
	}

	public Boolean isItemTextUnderline() {
		return itemTextUnderline;
	}

	public void setItemTextUnderline(Boolean itemTextUnderline) {
		this.itemTextUnderline = itemTextUnderline;
	}
}
