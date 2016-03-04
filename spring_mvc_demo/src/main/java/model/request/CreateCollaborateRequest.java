package model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import model.CollaborateMember;
import model.IRequestBase;
import model.PathRequestBase;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CreateCollaborateRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("is_visible")
	private boolean isVisible = true;
	private List<CollaborateMember> members;

	public CreateCollaborateRequest() {
		super();
	}

	public CreateCollaborateRequest(IRequestBase requestBase, String path, String sourcePath, String sharedRootId, String ownerId) {
		super();
		this.fromRequestBase(requestBase);
		this.setPath(path);
		this.setSourcePath(sourcePath);
		this.setSharedRootId(sharedRootId);
		this.setOwnerId(ownerId);
	}

	public List<CollaborateMember> getMembers() {
		return members;
	}

	public void setMembers(List<CollaborateMember> members) {
		this.members = members;
	}

	public boolean isVisible() {
		return isVisible;
	}

	public void setVisible(boolean isVisible) {
		this.isVisible = isVisible;
	}

}
