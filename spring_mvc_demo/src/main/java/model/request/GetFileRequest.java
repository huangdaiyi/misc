package model.request;

import model.PathRequestBase;

public class GetFileRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;

	private String password;

	public GetFileRequest() {
		super();
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}
}
