package model.request;

import java.util.Map;

import model.PathRequestBase;

public class BulkDeleteRequestItem extends BulkActionRequestItem {

	private static final long serialVersionUID = 1L;
	
	private boolean extract;
	
	public Map<String, Object> getAttributeMap() {
		Map<String, Object> map = super.getAttributeMap();
		if(this.isFolder()) map.put("extract", this.isExtract());
		return map;
	}
	
	public DeleteFolderRequest toDeleteFolderRequest() {
		DeleteFolderRequest request = new DeleteFolderRequest();
		request.fromPathRequestBase(this);
		request.setExtract(this.isExtract());
		return request;
	}
	
	public PathRequestBase toDeleteFileRequest() {
		PathRequestBase request = new PathRequestBase();
		request.fromPathRequestBase(this);
		return request;
	}

	public boolean isExtract() {
		return extract;
	}

	public void setExtract(boolean extract) {
		this.extract = extract;
	}
	
}
