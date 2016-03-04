package model.request;

import java.util.List;

import model.CellPhone;
import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class InviteMemberBySMSRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("member_id")
	private int memberId;
	@JsonProperty("force_cellphones")
	private List<CellPhone> forceCellphones;

	public InviteMemberBySMSRequest() {
		super();
	}

	public int getMemberId() {
		return memberId;
	}

	public void setMemberId(int memberId) {
		this.memberId = memberId;
	}

	public List<CellPhone> getForceCellphones() {
		return forceCellphones;
	}

	public void setForceCellphones(List<CellPhone> forceCellphones) {
		this.forceCellphones = forceCellphones;
	}

}
