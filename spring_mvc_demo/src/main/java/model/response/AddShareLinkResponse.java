package model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AddShareLinkResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private String link;

	public AddShareLinkResponse() {
		super();
	}

	public String getLink() {
		return link;
	}

	public void setLink(String link) {
		this.link = link;
	}

}
