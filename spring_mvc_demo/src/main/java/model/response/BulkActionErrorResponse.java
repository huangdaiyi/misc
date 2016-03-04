package model.response;

import java.io.Serializable;
import java.util.Map;

import model.request.BulkActionRequestItem;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;


@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkActionErrorResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private int index;
	
	private Map<String, Object> attributes;
	
	public void fromRequest(BulkActionRequestItem bulkActionRequestItem) {
	    setAttributes(bulkActionRequestItem.getAttributeMap());
	}
	
	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
	}
	
	@JsonAnyGetter
	public Map<String, Object> getAttributes() {
		return attributes;
	}

	@JsonAnyGetter
	public void setAttributes(Map<String, Object> attributes) {
		this.attributes = attributes;
	}
}
