package model.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import model.Mail;

@JsonIgnoreProperties(ignoreUnknown = true)
public class InviteResponseByMail implements Serializable {
	private static final long serialVersionUID = 1L;

	@JsonProperty("member_id")
	private int memberId;
	private List<Mail> mails;

	public InviteResponseByMail() {
		super();
	}

	public void setMailStatusCodeForEachMails(int code) {
		if (mails == null) {
			return;
		}
		for (Mail mail : mails) {
			mail.setMailStatusCode(code);
		}
	}

	public int getMemberId() {
		return memberId;
	}

	public void setMemberId(int memberId) {
		this.memberId = memberId;
	}

	public List<Mail> getMails() {
		return mails;
	}

	public void setMails(List<Mail> mails) {
		this.mails = mails;
	}

}
