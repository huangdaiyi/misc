package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateFolderNoteRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	private String note;

	public UpdateFolderNoteRequest() {
		super();
	}

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

}
