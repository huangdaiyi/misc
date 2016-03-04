package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UpdateMemberPhotoRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("member_id")
	private long memberId;
	@JsonProperty("photo_block_id")
	private String photoBlockId;
	@JsonProperty("photo_size")
	private int photoSize;

	public UpdateMemberPhotoRequest() {
		super();
	}

	public long getMemberId() {
		return memberId;
	}

	public void setMemberId(long memberId) {
		this.memberId = memberId;
	}

	public String getPhotoBlockId() {
		return photoBlockId;
	}

	public void setPhotoBlockId(String photoBlockId) {
		this.photoBlockId = photoBlockId;
	}

	public int getPhotoSize() {
		return photoSize;
	}

	public void setPhotoSize(int photoSize) {
		this.photoSize = photoSize;
	}

}
