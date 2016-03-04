package model.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AlreadyReadResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private List<String> read = new ArrayList<String>();
	private List<String> unread = new ArrayList<String>();
	public List<String> getRead() {
		return read;
	}
	public void setRead(List<String> read) {
		this.read = read;
	}
	public List<String> getUnread() {
		return unread;
	}
	public void setUnread(List<String> unread) {
		this.unread = unread;
	}
	
	
	
}