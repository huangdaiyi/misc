package model;

import java.io.Serializable;

import model.request.UpdateReaderFileMetadataRequest;

public class ReaderFileMetadata implements Serializable {

	private static final long serialVersionUID = 1L;

	private String metadataIndexId;
	private boolean backup;
	private String convertTo;
	private boolean encrypted;
	private int totalPage;
	private int duration;
	private int width;
	private int height;
	private String version;
	private String note;

	public ReaderFileMetadata() {
		super();
	}

	public static ReaderFileMetadata parse(UpdateReaderFileMetadataRequest updateReaderFileMetadataRequest) {
		ReaderFileMetadata result = new ReaderFileMetadata();
		result.setConvertTo(updateReaderFileMetadataRequest.getConvertTo());
		result.setVersion(updateReaderFileMetadataRequest.getVersion());
		result.setEncrypted(updateReaderFileMetadataRequest.isEncrypted());
		result.setTotalPage(updateReaderFileMetadataRequest.getTotalPage());
		result.setDuration(updateReaderFileMetadataRequest.getDuration());
		result.setWidth(updateReaderFileMetadataRequest.getWidth());
		result.setHeight(updateReaderFileMetadataRequest.getHeight());
		return result;
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

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

}
