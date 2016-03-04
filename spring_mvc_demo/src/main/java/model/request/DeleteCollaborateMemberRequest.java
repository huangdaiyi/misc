package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class DeleteCollaborateMemberRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("member_id")
	private int memberId;

	public DeleteCollaborateMemberRequest() {
		super();
	}

	public int getMemberId() {
		return memberId;
	}

	public void setMemberId(int memberId) {
		this.memberId = memberId;
	}

}
