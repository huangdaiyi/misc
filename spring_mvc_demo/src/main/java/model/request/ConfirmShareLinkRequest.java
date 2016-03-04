package model.request;

import model.RequestBase;

public class ConfirmShareLinkRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String link;
	private String password;

	public ConfirmShareLinkRequest() {
		super();
	}

	public String getLink() {
		return link;
	}

	public void setLink(String link) {
		this.link = link;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

}
