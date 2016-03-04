package model;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import org.springframework.jdbc.core.RowMapper;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.base.Joiner;

import utils.CellphonesUtils;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CollaborateMember implements Serializable {

	private static final long serialVersionUID = 1L;

	private String nickname;
	@JsonProperty("display_name")
	private String displayName;
	private List<CellPhone> cellphones;
	private List<Mail> mails;
	private boolean exists;
	private Boolean accepted;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("shared_root_id")
	private String metadataIndexId;
	@JsonProperty("is_invited_by_sms")
	private boolean invitedBySMS;
	@JsonProperty("is_invited_by_mail")
	private boolean invitedByMail;
	@JsonProperty("member_id")
	private Long memberId;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("photo_block_id")
	private String photoBlockId = "";
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("photo_size")
	private Long photoSize = 0L;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("is_registered")
	@Deprecated
	private Boolean registered;
	@JsonProperty("invite_message")
	private String inviteMessage = "";

	public CollaborateMember() {
		super();
	}

	@Deprecated
	public static RowMapper<CollaborateMember> getMapper() {
		return new RowMapper<CollaborateMember>() {
			@Override
			public CollaborateMember mapRow(ResultSet rs, int rowNum) throws SQLException {
				CollaborateMember member = new CollaborateMember();
				List<CellPhone> cellPhones = CellphonesUtils.buildCellphones(rs.getString("cellphones"));
				member.setCellphones(cellPhones);
				member.setMails(Mail.toMails(rs.getString("mails")));
				member.setMemberId(rs.getLong("member_id"));
				Boolean accept = null;
				if (rs.getObject("accepted") != null) {
					accept = rs.getBoolean("accepted");
				}
				member.setAccepted(accept);
				member.setInvitedByMail(rs.getBoolean("is_invited_by_mail"));
				member.setNickname(rs.getString("nickname"));
				member.setPhotoBlockId(rs.getString("photo_block_id"));
				member.setPhotoSize(rs.getLong("photo_size"));
				member.setMetadataIndexId(rs.getString("metadata_index_id"));
				member.setInviteMessage(rs.getString("invite_message"));
				return member;
			}
		};
	}

	public void updateSmsStatusCodeForEachCellphones(int code) {
		if (cellphones == null) {
			return;
		}
		for (CellPhone cellphone : cellphones) {
			cellphone.setSmsStatusCode(code);
		}
	}

	public void updateMailStatusCodeForEachMails(int code) {
		if (mails == null) {
			return;
		}
		for (Mail mail : mails) {
			mail.setMailStatusCode(code);
		}
	}

	public String generateCellphones() {
		if (cellphones == null) {
			return "";
		}
		return CellphonesUtils.composeCellphones(cellphones);
	}

	public String generateMails() {
		if (mails == null) {
			return "";
		}
		return Joiner.on("/").join(mails);
	}

	public String getNickname() {
		return nickname;
	}

	public void setNickname(String nickname) {
		this.nickname = nickname;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public List<CellPhone> getCellphones() {
		return cellphones;
	}

	public void setCellphones(List<CellPhone> cellphones) {
		this.cellphones = cellphones;
	}

	public List<Mail> getMails() {
		return mails;
	}

	public void setMails(List<Mail> mails) {
		this.mails = mails;
	}

	public Boolean getAccepted() {
		return accepted;
	}

	public boolean isExists() {
		return exists;
	}

	public void setExists(boolean exists) {
		this.exists = exists;
	}

	public void setAccepted(Boolean accepted) {
		this.accepted = accepted;
	}

	public boolean isInvitedByMail() {
		return invitedByMail;
	}

	public void setInvitedByMail(boolean invitedByMail) {
		this.invitedByMail = invitedByMail;
	}

	public Long getMemberId() {
		return memberId;
	}

	public void setMemberId(Long memberId) {
		this.memberId = memberId;
	}

	public String getPhotoBlockId() {
		return photoBlockId;
	}

	public void setPhotoBlockId(String photoBlockId) {
		this.photoBlockId = photoBlockId;
	}

	public Long getPhotoSize() {
		return photoSize;
	}

	public void setPhotoSize(Long photoSize) {
		this.photoSize = photoSize;
	}

	public String getMetadataIndexId() {
		return metadataIndexId;
	}

	public void setMetadataIndexId(String metadataIndexId) {
		this.metadataIndexId = metadataIndexId;
	}

	@Deprecated
	public Boolean getRegistered() {
		return registered;
	}

	@Deprecated
	public void setRegistered(Boolean registered) {
		this.registered = registered;
	}

	public String getInviteMessage() {
		return inviteMessage;
	}

	public void setInviteMessage(String inviteMessage) {
		this.inviteMessage = inviteMessage;
	}

	public boolean isInvitedBySMS() {
		return invitedBySMS;
	}

	public void setInvitedBySMS(boolean invitedBySMS) {
		this.invitedBySMS = invitedBySMS;
	}

}