package model;

public class CountAffected {

	private String parentId;
	private int fileCount;
	private int folderCount;
	private long totalSize;
	private int collaborateCount;
	private int syncRootCount;
	private int refLinkCount;

	// Constructors

	public CountAffected() {
		super();
	}

	public CountAffected(String parentId, int fileCount, int folderCount,
			long totalSize, int collaborateCount, int syncRootCount,
			int refLinkCount) {
		super();
		this.parentId = parentId;
		this.fileCount = fileCount;
		this.folderCount = folderCount;
		this.totalSize = totalSize;
		this.collaborateCount = collaborateCount;
		this.syncRootCount = syncRootCount;
		this.refLinkCount = refLinkCount;
	}

	public CountAffected(BaseMetadata metadata) {
		super();
		this.parentId = metadata.getParentId();
		this.fileCount = metadata.getFilesCount();
		this.folderCount = metadata.getFoldersCount();
		this.totalSize = metadata.getTotalSize();
		this.collaborateCount = metadata.getCollaborateCount();
		this.syncRootCount = metadata.getSyncRootCount();
		this.refLinkCount = metadata.getRefLinkCount();
	}

	public CountAffected(String parentId) {
		super();
		this.parentId = parentId;
	}

	// Transform

	public BaseMetadata toBaseMetadata() {
		BaseMetadata metadata = new BaseMetadata();
		metadata.setId(this.parentId);
		metadata.setFilesCount(this.fileCount);
		metadata.setFoldersCount(this.folderCount);
		metadata.setSize(this.totalSize);
		metadata.setCollaborateCount(this.collaborateCount);
		metadata.setSyncRootCount(this.syncRootCount);
		metadata.setRefLinkCount(this.refLinkCount);
		return metadata;
	}

	// Cumulative modifications

	public void addFolderCount(int count) {
		this.folderCount += count;
	}

	public void addFileCount(int count) {
		this.fileCount += count;
	}

	public void addSize(long size) {
		this.totalSize += size;
	}

	public void addCollaborateCount(int count) {
		this.collaborateCount = count;
	}

	// Getters & Setters

	public String getParentId() {
		return parentId;
	}

	public void setParentId(String parentId) {
		this.parentId = parentId;
	}

	public int getFileCount() {
		return fileCount;
	}

	public void setFileCount(int fileCount) {
		this.fileCount = fileCount;
	}

	public int getFolderCount() {
		return folderCount;
	}

	public void setFolderCount(int folderCount) {
		this.folderCount = folderCount;
	}

	public long getTotalSize() {
		return totalSize;
	}

	public void setTotalSize(long totalSize) {
		this.totalSize = totalSize;
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

	public int getRefLinkCount() {
		return refLinkCount;
	}

	public void setRefLinkCount(int refLinkCount) {
		this.refLinkCount = refLinkCount;
	}
}
