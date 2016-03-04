package model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import model.RequestBase;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateUserDiskSpaceRequest extends RequestBase {
	private static final long serialVersionUID = 1L;

	@JsonProperty("access_key")
	private String accessKey;
	private Long total;

	public UpdateUserDiskSpaceRequest() {
		super();
	}

	public String getAccessKey() {
		return accessKey;
	}

	public void setAccessKey(String accessKey) {
		this.accessKey = accessKey;
	}

	public Long getTotal() {
		return total;
	}

	public void setTotal(Long total) {
		this.total = total;
	}

}