package guava.demo;

import java.util.List;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;

public class BackupResult {

	public BackupResult(int foldersCount, int filesCount, long size,
			String metadataId, String parentId, boolean isFolder) {
		super();
		this.foldersCount = foldersCount;
		this.filesCount = filesCount;
		this.size = size;
		this.metadataId = metadataId;
		this.isFolder = isFolder;
		this.parentId = parentId;
	}

	
	
	public BackupResult() {
	}

	private int foldersCount;
	private int filesCount;
	private long size;
	private String metadataId;
	private String parentId;
	private boolean isFolder;

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

	public boolean isFolder() {
		return isFolder;
	}

	public void setFolder(boolean isFolder) {
		this.isFolder = isFolder;
	}

	public String getParentId() {
		return parentId;
	}

	public void setParentId(String parentId) {
		this.parentId = parentId;
	}

	public void addCount(int count, boolean isFolder) {
		if (isFolder()) {
			this.filesCount += count;
		} else {
			this.filesCount += count;
		}
	}

	public void addSize(long size) {
		this.size += size;
	}

}
