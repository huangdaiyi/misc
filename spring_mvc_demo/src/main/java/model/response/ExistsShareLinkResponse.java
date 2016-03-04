package model.response;

import java.io.Serializable;

public class ExistsShareLinkResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private String link;

	public ExistsShareLinkResponse() {
		super();
	}

	public String getLink() {
		return link;
	}

	public void setLink(String link) {
		this.link = link;
	}

}
