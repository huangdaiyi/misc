package exception;

import constants.HttpStatus;

public class MetadataException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	private HttpStatus httpStatus;
	private Object responseData;

	public MetadataException(HttpStatus httpStatus) {
		this.httpStatus = httpStatus;
		this.responseData = null;
	}
	
	public MetadataException(HttpStatus httpStatus, Object responseData) {
		this.httpStatus = httpStatus;
		this.responseData = responseData;
	}

	public HttpStatus getHttpStatus() {
		return httpStatus;
	}

	public void setHttpStatus(HttpStatus httpStatus) {
		this.httpStatus = httpStatus;
	}

	public Object getResponseData() {
		return responseData;
	}

	public void setResponseData(Object responseData) {
		this.responseData = responseData;
	}
}
