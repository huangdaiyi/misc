package model;

import java.util.List;
import java.util.Map;

public class NewIdAndMapId {
	private List<List<Metadata>> result;
	private List<SourceTargetMap> fileMapIds;
	private List<SourceTargetMap> allMapIds;
	private Map<String, FolderMapItem> folderMapIds;
	private List<SourceTargetMap> shardFolderIdMap;

	public List<List<Metadata>> getResult() {
		return result;
	}

	public void setResult(List<List<Metadata>> result) {
		this.result = result;
	}

	public List<SourceTargetMap> getFileMapIds() {
		return fileMapIds;
	}

	public void setFileMapIds(List<SourceTargetMap> fileMapIds) {
		this.fileMapIds = fileMapIds;
	}

	public Map<String, FolderMapItem> getFolderMapIds() {
		return folderMapIds;
	}

	public void setFolderMapIds(Map<String, FolderMapItem> folderMapIds) {
		this.folderMapIds = folderMapIds;
	}

	public List<SourceTargetMap> getShardFolderIdMap() {
		return shardFolderIdMap;
	}

	public void setShardFolderIdMap(List<SourceTargetMap> shardFolderIdMap) {
		this.shardFolderIdMap = shardFolderIdMap;
	}

	public List<SourceTargetMap> getAllMapIds() {
		return allMapIds;
	}

	public void setAllMapIds(List<SourceTargetMap> allMapIds) {
		this.allMapIds = allMapIds;
	}

}
