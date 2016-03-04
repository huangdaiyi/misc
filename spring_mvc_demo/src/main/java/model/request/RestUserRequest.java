package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;


@JsonIgnoreProperties(ignoreUnknown = true)
public class RestUserRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	public RestUserRequest() {
		super();
	}

}
