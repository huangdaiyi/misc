package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetFolderNoteResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private String note;

	public GetFolderNoteResponse() {
		super();
	}

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

}
