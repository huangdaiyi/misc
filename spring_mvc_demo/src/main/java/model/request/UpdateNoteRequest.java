package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateNoteRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;

	private String note;

	public UpdateNoteRequest() {
		super();
	}

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}
}
