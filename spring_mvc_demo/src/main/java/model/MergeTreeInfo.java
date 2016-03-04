package model;

import java.util.List;

public class MergeTreeInfo {

	private List<List<Metadata>> nodesLevel;
	private List<Metadata> parentNodes;
	private List<String> AllParentId;
	private List<String> All_subId;
	private String oldSharedUpperPath = "";

	public String getOldSharedUpperPath() {
		return oldSharedUpperPath;
	}

	public void setOldSharedUpperPath(String oldSharedUpperPath) {
		this.oldSharedUpperPath = oldSharedUpperPath;
	}

	public List<List<Metadata>> getNodesLevel() {
		return nodesLevel;
	}

	public void setNodesLevel(List<List<Metadata>> nodesLevel) {
		this.nodesLevel = nodesLevel;
	}

	public List<String> getAllParentId() {
		return AllParentId;
	}

	public void setAllParentId(List<String> allParentId) {
		AllParentId = allParentId;
	}

	public List<String> getAll_subId() {
		return All_subId;
	}

	public void setAll_subId(List<String> all_subId) {
		All_subId = all_subId;
	}

	public List<Metadata> getParentNodes() {
		return parentNodes;
	}

	public void setParentNodes(List<Metadata> parentNodes) {
		this.parentNodes = parentNodes;
	}
}
