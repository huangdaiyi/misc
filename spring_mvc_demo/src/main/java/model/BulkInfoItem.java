package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkInfoItem implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String id;
	private String score;
	private String highlights;
	
	public BulkInfoItem() {
		super();
	}

	public BulkInfoItem(BulkInfoItem bulkInfoItem) {
		if(bulkInfoItem != null) {
			this.id = bulkInfoItem.getId();
			this.score = bulkInfoItem.getScore();
			this.highlights = bulkInfoItem.getHighlights();
		}
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getScore() {
		return score;
	}

	public void setScore(String score) {
		this.score = score;
	}

	public String getHighlights() {
		return highlights;
	}

	public void setHighlights(String highlights) {
		this.highlights = highlights;
	}

}
