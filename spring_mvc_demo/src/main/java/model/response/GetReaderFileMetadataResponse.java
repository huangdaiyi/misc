package model.response;

import java.io.Serializable;
import java.util.List;

import model.ReaderFileMetadata;
import model.ReaderPageDetail;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetReaderFileMetadataResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonProperty("convert_to")
	private String convertTo;
	private String version;
	private boolean encrypted;
	@JsonProperty("total_page")
	private int totalPage;
	private int duration;
	private int width;
	private int height;
	private String note;
	@JsonProperty("page_details")
	private List<ReaderPageDetail> readerPageDetails;

	public GetReaderFileMetadataResponse() {
		super();
	}

	public static GetReaderFileMetadataResponse parse(ReaderFileMetadata readerFileMetadata, List<ReaderPageDetail> readerPageDetails) {
		GetReaderFileMetadataResponse response = new GetReaderFileMetadataResponse();
		response.setConvertTo(readerFileMetadata.getConvertTo());
		response.setVersion(readerFileMetadata.getVersion());
		response.setEncrypted(readerFileMetadata.isEncrypted());
		response.setTotalPage(readerFileMetadata.getTotalPage());
		response.setDuration(readerFileMetadata.getDuration());
		response.setWidth(readerFileMetadata.getWidth());
		response.setHeight(readerFileMetadata.getHeight());
		response.setNote(readerFileMetadata.getNote());
		response.setReaderPageDetails(readerPageDetails);
		return response;
	}

	public String getConvertTo() {
		return convertTo;
	}

	public void setConvertTo(String convertTo) {
		this.convertTo = convertTo;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
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

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

	public List<ReaderPageDetail> getReaderPageDetails() {
		return readerPageDetails;
	}

	public void setReaderPageDetails(List<ReaderPageDetail> readerPageDetails) {
		this.readerPageDetails = readerPageDetails;
	}

}
