package model.request;

import java.util.List;

import model.CollaborateMember;
import model.PathRequestBase;

public class AddCollaborateMemberRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	private List<CollaborateMember> members;

	public AddCollaborateMemberRequest() {
		super();
	}

	public List<CollaborateMember> getMembers() {
		return members;
	}

	public void setMembers(List<CollaborateMember> members) {
		this.members = members;
	}
}
