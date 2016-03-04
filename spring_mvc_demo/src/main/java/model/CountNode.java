package model;

import java.util.List;

public class CountNode {
	private List<String> allParentId; //must
	private String leafId;//must
	private String fatherId;//deleteFolder:must, other:optional
	private Long leafSize;
	private int leafFolderCount;//when add folder, leafFolderCount=0
	private int leafFileCount;//when add file, leafFileCount=0
	private boolean isFolder;//must
	private boolean isBackup;//must
	private int collaborateCount;
	private int syncRootCount;

	public List<String> getAllParentId() {
		return allParentId;
	}

	public void setAllParentId(List<String> allParentId) {
		this.allParentId = allParentId;
	}

	public String getLeafId() {
		return leafId;
	}

	public void setLeafId(String leafId) {
		this.leafId = leafId;
	}

	public String getFatherId() {
		return fatherId;
	}

	public void setFatherId(String fatherId) {
		this.fatherId = fatherId;
	}

	public Long getLeafSize() {
		return leafSize;
	}

	public void setLeafSize(Long leafSize) {
		this.leafSize = leafSize;
	}

	public int getLeafFolderCount() {
		return leafFolderCount;
	}

	public void setLeafFolderCount(int leafFolderCount) {
		this.leafFolderCount = leafFolderCount;
	}

	public int getLeafFileCount() {
		return leafFileCount;
	}

	public void setLeafFileCount(int leafFileCount) {
		this.leafFileCount = leafFileCount;
	}

	public boolean isFolder() {
		return isFolder;
	}

	public void setFolder(boolean isFolder) {
		this.isFolder = isFolder;
	}

	public boolean isBackup() {
		return isBackup;
	}

	public void setBackup(boolean isBackup) {
		this.isBackup = isBackup;
	}

	public int getCollaborateCount() {
		return collaborateCount;
	}

	public void setCollaborateCount(int collaborateCount) {
		this.collaborateCount = collaborateCount;
	}

	public int getSyncRootCount() {
		return syncRootCount;
	}

	public void setSyncRootCount(int syncRootCount) {
		this.syncRootCount = syncRootCount;
	}
}
