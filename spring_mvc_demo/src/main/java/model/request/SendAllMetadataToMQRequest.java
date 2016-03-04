package model.request;

import model.RequestBase;

public class SendAllMetadataToMQRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String filter;
	private String[] extensions;

	public SendAllMetadataToMQRequest() {
		super();
	}

	public String getFilter() {
		return filter;
	}

	public void setFilter(String filter) {
		this.filter = filter;
	}

	public String[] getExtensions() {
		return extensions;
	}

	public void setExtensions(String[] extensions) {
		this.extensions = extensions;
	}

}
