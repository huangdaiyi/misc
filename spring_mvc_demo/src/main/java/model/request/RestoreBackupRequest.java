package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class RestoreBackupRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	private String name;

	public RestoreBackupRequest() {
		super();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

}
