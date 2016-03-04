package model.request;

import java.util.HashMap;
import java.util.Map;

import model.IRequestBase;
import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BulkActionRequestItem extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("is_folder")
	private boolean folder;
	
	public void fromRequestBase(IRequestBase requestBase) {
		this.setToken(requestBase.getToken());
		this.setUserId(requestBase.getUserId());
		this.setDeviceId(requestBase.getDeviceId());
		this.setDbHost(requestBase.getDbHost());
		this.setDbPort(requestBase.getDbPort());
		this.setDbInstance(requestBase.getDbInstance());
		this.setDbServerName(requestBase.getDbServerName());
	}
	
	public void fromBulkActionRequestItem(BulkActionRequestItem bulkActionRequestItem) {
		this.fromRequestBase(bulkActionRequestItem);
		this.setPath(bulkActionRequestItem.getPath());
		this.setSourcePath(bulkActionRequestItem.getSourcePath());
		this.setOwnerId(bulkActionRequestItem.getOwnerId());
		this.setSharedRootId(bulkActionRequestItem.getSharedRootId());
		this.setFolder(bulkActionRequestItem.isFolder());
	}

	public Map<String, Object> getAttributeMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("is_folder", folder);
		if(this.getPath() != null) map.put("path", this.getPath());
		if(this.getSourcePath() != null) map.put("source_path", this.getSourcePath());
		if(this.getOwnerId() != null) map.put("owner_id", this.getOwnerId());
		if(this.getSharedRootId() != null) map.put("shared_root_id", this.getSharedRootId());
		return map;
	}
	
	public PathRequestBase toRequest() {
		return null;
	}
	
	public boolean isFolder() {
		return folder;
	}
	
	public void setFolder(boolean folder) {
		this.folder = folder;
	}
	
}
