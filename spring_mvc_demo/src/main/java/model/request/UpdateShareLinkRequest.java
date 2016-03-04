package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UpdateShareLinkRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("is_folder")
	private boolean folder;
	private String password;
	private String expired;

	public UpdateShareLinkRequest() {
		super();
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getExpired() {
		return expired;
	}

	public void setExpired(String expired) {
		this.expired = expired;
	}

}
