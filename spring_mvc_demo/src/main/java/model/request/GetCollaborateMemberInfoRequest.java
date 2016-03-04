package model.request;

import java.util.List;

import model.CellPhone;
import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

public class GetCollaborateMemberInfoRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("member_id")
	private int memberId;
	private String nickname;
	private List<CellPhone> cellphones;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private Boolean detail;

	public GetCollaborateMemberInfoRequest() {
		super();
	}

	public int getMemberId() {
		return memberId;
	}

	public void setMemberId(int memberId) {
		this.memberId = memberId;
	}

	public String getNickname() {
		return nickname;
	}

	public void setNickname(String nickname) {
		this.nickname = nickname;
	}

	public List<CellPhone> getCellphones() {
		return cellphones;
	}

	public void setCellphones(List<CellPhone> cellphones) {
		this.cellphones = cellphones;
	}

	public Boolean getDetail() {
		if (this.detail == null) {
			this.detail = false;
		}
		return detail;
	}

	public void setDetail(Boolean detail) {
		this.detail = detail;
	}

}
