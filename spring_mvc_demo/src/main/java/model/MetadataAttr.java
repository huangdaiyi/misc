package model;

public class MetadataAttr {

	private String metadataIndexId = "";
	private boolean backup = false;
	private int foldersCount = 0;
	private int filesCount = 0;
	private long totalSize = 0;
	private int width = 0;
	private int height = 0;
	private int collaborateCount = 0;
	private int syncRootCount = 0;
	private int refLinkCount = 0;

	public MetadataAttr(String metadataIndexId, boolean backup,
			int foldersCount, int filesCount, long totalSize, int width,
			int height, int collaborateCount, int syncRootCount, int refLinkCount) {
		super();
		this.metadataIndexId = metadataIndexId;
		this.backup = backup;
		this.foldersCount = foldersCount;
		this.filesCount = filesCount;
		this.totalSize = totalSize;
		this.width = width;
		this.height = height;
		this.collaborateCount = collaborateCount;
		this.syncRootCount = syncRootCount;
		this.refLinkCount = refLinkCount;
	}

	public MetadataAttr(String metadataIndexId, boolean backup,
			int foldersCount, int filesCount, long totalSize) {
		this(metadataIndexId, backup, foldersCount, filesCount, totalSize, 0, 0);
	}

	public MetadataAttr(String metadataIndexId, boolean backup) {
		this(metadataIndexId, backup, 0, 0, 0L, 0, 0);
	}

	public MetadataAttr(String metadataIndexId, boolean backup, int width, int height) {
		this(metadataIndexId, backup, 0, 0, 0L, width, height);
	}
	
	public MetadataAttr(String metadataIndexId, boolean backup,
			int foldersCount, int filesCount, long totalSize, int width,
			int height){
		this(metadataIndexId, backup, 0, 0, 0L, width, height, 0, 0, 0);
	}
	
	public CountAffected toCountAffected() {
		CountAffected countAffected = new CountAffected();
		countAffected.setParentId(metadataIndexId);
		countAffected.setFileCount(filesCount);
		countAffected.setFolderCount(foldersCount);
		countAffected.setTotalSize(totalSize);
		countAffected.setCollaborateCount(collaborateCount);
		countAffected.setSyncRootCount(syncRootCount);
		countAffected.setRefLinkCount(refLinkCount);
		return countAffected;
	}
	
	public MetadataAttr() {
		super();
	}

	public String getMetadataIndexId() {
		return metadataIndexId;
	}

	public void setMetadataIndexId(String metadataIndexId) {
		this.metadataIndexId = metadataIndexId;
	}

	public boolean isBackup() {
		return backup;
	}

	public void setBackup(boolean backup) {
		this.backup = backup;
	}

	public int getFoldersCount() {
		return foldersCount;
	}

	public void setFoldersCount(int foldersCount) {
		this.foldersCount = foldersCount;
	}

	public int getFilesCount() {
		return filesCount;
	}

	public void setFilesCount(int filesCount) {
		this.filesCount = filesCount;
	}

	public long getTotalSize() {
		return totalSize;
	}

	public void setTotalSize(long totalSize) {
		this.totalSize = totalSize;
	}

	public int getWidth() {
		return width;
	}

	public void setWidth(int width) {
		this.width = width;
	}

	public int getHeight() {
		return height;
	}

	public void setHeight(int height) {
		this.height = height;
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
