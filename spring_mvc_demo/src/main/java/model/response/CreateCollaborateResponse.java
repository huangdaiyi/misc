package model.response;

import java.io.Serializable;
import java.util.List;

import model.CollaborateMember;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CreateCollaborateResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private String path;
	private String owner;
	@JsonProperty("owner_id")
	private String ownerId;
	private Boolean accepted;
	@JsonProperty("member_id")
	private String memberId;
	@JsonProperty("shared_root_id")
	private String sharedRootId  = "";
	private List<CollaborateMember> members;

	public CreateCollaborateResponse() {
		super();
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public Boolean getAccepted() {
		return accepted;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getOwner() {
		return owner;
	}

	public void setOwner(String owner) {
		this.owner = owner;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}

	public void setAccepted(Boolean accepted) {
		this.accepted = accepted;
	}

	public String getMemberId() {
		return memberId;
	}

	public void setMemberId(String memberId) {
		this.memberId = memberId;
	}

	public List<CollaborateMember> getMembers() {
		return members;
	}

	public void setMembers(List<CollaborateMember> members) {
		this.members = members;
	}

	public String getSharedRootId() {
		return sharedRootId;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}

}
