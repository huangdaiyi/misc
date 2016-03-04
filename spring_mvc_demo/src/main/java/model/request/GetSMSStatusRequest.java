package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetSMSStatusRequest extends RequestBase {
	
	private static final long serialVersionUID = 1L;
	
	private String sid;
	
	public GetSMSStatusRequest() {
		super();
	}

	public String getSid() {
		return sid;
	}

	public void setSid(String sid) {
		this.sid = sid;
	}
	
}
