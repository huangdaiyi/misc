package model;

import org.jivesoftware.smack.util.StringUtils;

import com.fasterxml.jackson.annotation.JsonProperty;

import constants.HttpStatus;
import constants.InviteType;

public class InviteProcessingStatus {

	@JsonProperty("member_id")
	private Long memberId;
	@JsonProperty("invite_to")
	private String inviteTo;
	private InviteType type;
	@JsonProperty("sms_sid")
	private String smsSid;
	private String status;

	public InviteProcessingStatus() {
		super();
	}

	public InviteProcessingStatus(Long memberId, String inviteTo, InviteType type, String smsSid, String status) {
		super();
		this.memberId = memberId;
		this.inviteTo = inviteTo;
		this.type = type;
		this.smsSid = smsSid;
		this.status = status;
	}

	public boolean isCellphoneEquals(CellPhone cellphone) {
		if (cellphone == null || inviteTo == null || inviteTo.isEmpty()) {
			return false;
		}
		return (type == InviteType.CELLPHONE && inviteTo.equalsIgnoreCase(cellphone.toString()));
	}

	public boolean isMailEquals(Mail mail) {
		if (mail == null || inviteTo == null || inviteTo.isEmpty()) {
			return false;
		}
		return (type == InviteType.EMAIL && inviteTo.equalsIgnoreCase(mail.toString()));
	}

	public Integer toStatusCode() {
		if ("success".equals(status)) {
			return HttpStatus.OK.getCode();
		} else if ("undelivered".equals(status)) {
			return HttpStatus.SMS_UNDELIVERED.getCode();
		} else if (StringUtils.isNullOrEmpty(status) == false) {
			return HttpStatus.INTERNAL_SERVER_ERROR.getCode();
		}
		return null;
	}

	public void setTypeString(String typeString) {
		this.type = InviteType.parse(typeString);
	}

	public Long getMemberId() {
		return memberId;
	}

	public void setMemberId(Long memberId) {
		this.memberId = memberId;
	}

	public String getInviteTo() {
		return inviteTo;
	}

	public void setInviteTo(String inviteTo) {
		this.inviteTo = inviteTo;
	}

	public InviteType getType() {
		return type;
	}

	public void setType(InviteType type) {
		this.type = type;
	}

	public String getSmsSid() {
		return smsSid;
	}

	public void setSmsSid(String smsSid) {
		this.smsSid = smsSid;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

}