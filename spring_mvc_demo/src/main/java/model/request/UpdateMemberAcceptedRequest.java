package model.request;

import model.PathRequestBase;

public class UpdateMemberAcceptedRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;


	private Boolean accepted;

	public UpdateMemberAcceptedRequest() {
		super();
	}
	public Boolean isAccepted() {
		return accepted;
	}

	public void setAccepted(Boolean accepted) {
		this.accepted = accepted;
	}

}
