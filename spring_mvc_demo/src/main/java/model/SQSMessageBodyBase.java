package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class SQSMessageBodyBase implements Serializable {
	private static final long serialVersionUID = 1L;

	private String action;

	public String getAction() {
		return action == null ? "" : action;
	}

	public void setAction(String action) {
		this.action = action;
	}
	
}
