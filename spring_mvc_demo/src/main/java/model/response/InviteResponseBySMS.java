package model.response;

import java.io.Serializable;
import java.util.List;

import model.CellPhone;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class InviteResponseBySMS implements Serializable {
	private static final long serialVersionUID = 1L;

	@JsonProperty("member_id")
	private int memberId;
	@JsonProperty("cellphones")
	private List<CellPhone> cellphones;

	public InviteResponseBySMS() {
		super();
	}

	public void setSmsStatusCodeForEachCellphones(int code) {
		if (cellphones == null) {
			return;
		}
		for (CellPhone cellPhone : cellphones) {
			cellPhone.setSmsStatusCode(code);
		}
	}

	public int getMemberId() {
		return memberId;
	}

	public void setMemberId(int memberId) {
		this.memberId = memberId;
	}

	public List<CellPhone> getCellphones() {
		return cellphones;
	}

	public void setCellphones(List<CellPhone> cellphones) {
		this.cellphones = cellphones;
	}

}
