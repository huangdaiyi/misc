package model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import model.Mail;
import model.RequestBase;

@JsonIgnoreProperties(ignoreUnknown = true)
public class InviteMemberByMailRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("member_id")
	private int memberId;
	@JsonProperty("force_mails")
	private List<Mail> forceMails;

	public InviteMemberByMailRequest() {
		super();
	}

	public int getMemberId() {
		return memberId;
	}

	public void setMemberId(int memberId) {
		this.memberId = memberId;
	}

	public List<Mail> getForceMails() {
		return forceMails;
	}

	public void setForceMails(List<Mail> forceMails) {
		this.forceMails = forceMails;
	}

}
