package constants;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum InviteType {
	
	EMAIL("email"),
	CELLPHONE("cellphone");

	private String inviteType;
	
	private InviteType(String inviteType){
		this.inviteType = inviteType;
	}
	
	@JsonCreator
	public static InviteType parse(String inviteType) {
		for (InviteType ps : InviteType.values()) {
			if (ps.getInviteType().equals(inviteType)) {
				return ps;
			}
		}
		return null;
	}

	@JsonValue
	public String getInviteType() {
		return inviteType;
	}

	public void setInviteType(String inviteType) {
		this.inviteType = inviteType;
	}

	@Override
	public String toString() {
		return inviteType;
	}

}
