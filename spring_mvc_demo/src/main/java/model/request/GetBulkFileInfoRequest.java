package model.request;

import java.util.List;

import model.BulkFileInfoItem;
import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetBulkFileInfoRequest extends PathRequestBase {
	
	private static final long serialVersionUID = 1;

	
	private List<BulkFileInfoItem> blocks;
	
	public GetBulkFileInfoRequest() {
		super();
	}

	public List<BulkFileInfoItem> getBlocks() {
		return blocks;
	}
	public void setBlocks(List<BulkFileInfoItem> blocks) {
		this.blocks = blocks;
	}

}
