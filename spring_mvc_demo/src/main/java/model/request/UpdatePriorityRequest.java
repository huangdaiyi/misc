package model.request;

import java.util.List;

import model.PathRequestBase;
import model.PriorityInfo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdatePriorityRequest extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	private List<PriorityInfo> priority;

	public UpdatePriorityRequest() {
		super();
	}

	public List<PriorityInfo> getPriority() {
		return priority;
	}

	public void setPriority(List<PriorityInfo> priority) {
		this.priority = priority;
	}

}
