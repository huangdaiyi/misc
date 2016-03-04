package model;

import constants.AffectTag;

public class MetaAffectResult {

	private int foldersCount;
	private int filesCount;
	private long size;
	private String metadataId;
	private String parentId;
	private boolean folder;
	private AffectTag tag;
	private int width;
	private int height;

	public MetaAffectResult() {
		super();
	}

	public MetaAffectResult(String metadataId, String parentId, boolean folder, int width, int height) {
		super();
		this.metadataId = metadataId;
		this.parentId = parentId;
		this.folder = folder;
		this.foldersCount = 0;
		this.filesCount = 0;
		this.size = 0L;
		this.tag = AffectTag.NONE;
		this.width = width;
		this.height = height;
	}

	public void addFolderCount(int count) {
		this.foldersCount += count;
	}

	public void addFileCount(int count) {
		this.filesCount += count;
	}

	public void addSize(long size) {
		this.size += size;
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

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	public String getMetadataId() {
		return metadataId;
	}

	public void setMetadataId(String metadataId) {
		this.metadataId = metadataId;
	}

	public String getParentId() {
		return parentId;
	}

	public void setParentId(String parentId) {
		this.parentId = parentId;
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

	public AffectTag getTag() {
		return tag;
	}

	public void setTag(AffectTag tag) {
		this.tag = tag;
	}

	public boolean isAffect() {
		return this.tag != AffectTag.NONE || this.filesCount != 0
				|| this.foldersCount != 0 || this.size != 0L;
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

	public MetadataAttr toMetadataAdditionalAttr(boolean isBackup) {
		return new MetadataAttr(this.getMetadataId(), isBackup,
				this.getFoldersCount(), this.getFilesCount(), this.getSize(), this.getWidth(), this.getHeight());
	}

}
