package model.request;

import model.RequestBase;

public class AddExtensionBackupRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String extension;
	
	public AddExtensionBackupRequest() {
		super();
	}

	public String getExtension() {
		return extension;
	}

	public void setExtension(String extension) {
		this.extension = extension;
	}

}
