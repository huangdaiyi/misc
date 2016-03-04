package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class GetCollaborateInfoRequest extends RequestBase {

	private static final long serialVersionUID = 1L;


	@JsonProperty("shared_root_id")
	private String shareRootId;
	@JsonProperty("detail")
	private boolean detail;
	
	public String getShareRootId() {
		return shareRootId;
	}


	public void setShareRootId(String shareRootIdld) {
		this.shareRootId = shareRootIdld;
	}



	public GetCollaborateInfoRequest() {
		super();
	}


	public boolean isDetail() {
		return detail;
	}

	public void setDetail(boolean detail) {
		this.detail = detail;
	}

}
