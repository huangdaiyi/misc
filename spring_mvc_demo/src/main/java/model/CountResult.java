package model;

public class CountResult {

	private int folderCount = 0;
	private int fileCount = 0;
	private long fileSize = 0;
	private int collaborateCount;
	private int syncRootCount;

	public int getFolderCount() {
		return folderCount;
	}

	public void setFolderCount(int folderCount) {
		this.folderCount = folderCount;
	}

	public int getFileCount() {
		return fileCount;
	}

	public void setFileCount(int fileCount) {
		this.fileCount = fileCount;
	}

	public long getFileSize() {
		return fileSize;
	}

	public void setFileSize(long fileSize) {
		this.fileSize = fileSize;
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
