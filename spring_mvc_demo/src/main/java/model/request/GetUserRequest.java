package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetUserRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	public GetUserRequest() {
		super();
	}

}
