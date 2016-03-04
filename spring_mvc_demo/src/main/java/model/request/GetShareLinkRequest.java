package model.request;

import model.RequestBase;

public class GetShareLinkRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String link;
	private boolean detail;

	public GetShareLinkRequest() {
		super();
	}

	public String getLink() {
		return link;
	}

	public void setLink(String link) {
		this.link = link;
	}

	public boolean isDetail() {
		return detail;
	}

	public void setDetail(boolean detail) {
		this.detail = detail;
	}

}
