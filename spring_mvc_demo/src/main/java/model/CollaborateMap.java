package model;

import java.util.List;

public class CollaborateMap {
	private Metadata collaborateFolder;
	private CollaborateMember member;
	private List<CollaborateMember> members;
	private boolean CanAccessCollaborate = false;

	public Metadata getCollaborateFolder() {
		return collaborateFolder;
	}

	public void setCollaborateFolder(Metadata collaborateFolder) {
		this.collaborateFolder = collaborateFolder;
	}

	public CollaborateMember getMember() {
		return member;
	}

	public void setMember(CollaborateMember member) {
		this.member = member;
	}

	public boolean isCanAccessCollaborate() {
		return CanAccessCollaborate;
	}

	public void setCanAccessCollaborate(boolean canAccessCollaborate) {
		CanAccessCollaborate = canAccessCollaborate;
	}

	public List<CollaborateMember> getMembers() {
		return members;
	}

	public void setMembers(List<CollaborateMember> members) {
		this.members = members;
	}

}
