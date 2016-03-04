package model.request;

import java.util.List;

import model.RequestBase;

public class DeleteExtensionBackupRequest extends RequestBase {

	private static final long serialVersionUID = 1L;
	private List<Integer> id;

	
	public DeleteExtensionBackupRequest() {
		super();
	}


	public List<Integer> getId() {
		return id;
	}


	public void setId(List<Integer> id) {
		this.id = id;
	}


}
