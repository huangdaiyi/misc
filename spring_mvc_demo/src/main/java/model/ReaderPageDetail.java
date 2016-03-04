package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ReaderPageDetail implements Serializable {
	private static final long serialVersionUID = 1;

	@JsonProperty("id")
	private String metadataIndexId;
	@JsonProperty("is_backup")
	private boolean backup;
	@JsonProperty("page_no")
	private int pageNo;
	@JsonProperty("image_count")
	private int imageCount;
	@JsonProperty("sheet_name")
	private String sheetName;

	public ReaderPageDetail() {
		super();
	}

	public String getMetadataIndexId() {
		return metadataIndexId;
	}

	public void setMetadataIndexId(String metadataIndexId) {
		this.metadataIndexId = metadataIndexId;
	}

	public boolean isBackup() {
		return backup;
	}

	public void setBackup(boolean backup) {
		this.backup = backup;
	}

	public int getPageNo() {
		return pageNo;
	}

	public void setPageNo(int pageNo) {
		this.pageNo = pageNo;
	}

	public int getImageCount() {
		return imageCount;
	}

	public void setImageCount(int imageCount) {
		this.imageCount = imageCount;
	}

	public String getSheetName() {
		return sheetName;
	}

	public void setSheetName(String sheetName) {
		this.sheetName = sheetName;
	}

}
