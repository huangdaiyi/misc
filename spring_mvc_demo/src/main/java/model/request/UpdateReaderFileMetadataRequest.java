package model.request;

import java.util.List;

import model.PathRequestBase;
import model.ReaderPageDetail;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateReaderFileMetadataRequest extends PathRequestBase {

	private static final long serialVersionUID = 1;

	@JsonProperty("convert_to")
	private String convertTo;
	private boolean encrypted;
	@JsonProperty("total_page")
	private int totalPage;
	private int duration;
	private int width;
	private int height;
	private String version;
	@JsonProperty("page_details")
	private List<ReaderPageDetail> pageDetails;

	public UpdateReaderFileMetadataRequest() {
		super();
	}

	public String getConvertTo() {
		return convertTo;
	}

	public void setConvertTo(String convertTo) {
		this.convertTo = convertTo;
	}

	public boolean isEncrypted() {
		return encrypted;
	}

	public void setEncrypted(boolean encrypted) {
		this.encrypted = encrypted;
	}

	public int getTotalPage() {
		return totalPage;
	}

	public void setTotalPage(int totalPage) {
		this.totalPage = totalPage;
	}

	public int getDuration() {
		return duration;
	}

	public void setDuration(int duration) {
		this.duration = duration;
	}

	public int getWidth() {
		return width;
	}

	public void setWidth(int width) {
		this.width = width;
	}

	public int getHeight() {
		return height;
	}

	public void setHeight(int height) {
		this.height = height;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public List<ReaderPageDetail> getPageDetails() {
		return pageDetails;
	}

	public void setPageDetails(List<ReaderPageDetail> pageDetails) {
		this.pageDetails = pageDetails;
	}

}
