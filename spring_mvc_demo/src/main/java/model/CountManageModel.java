package model;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

public class CountManageModel {

	private Hashtable<String, MetadataAttr> tb = new Hashtable<String, MetadataAttr>();

	private MetadataAttr getOrCreateMetadataAttr(String id) {
		MetadataAttr attr = tb.get(id);
		if (attr == null) {
			attr = new MetadataAttr();
			attr.setMetadataIndexId(id);
		}
		return attr;
	}
	
	public List<CountAffected> getAffected() {
		List<CountAffected> list = new ArrayList<CountAffected>();
		for (Iterator<String> it = tb.keySet().iterator(); it.hasNext();) {
			String key = it.next();
			list.add(tb.get(key).toCountAffected());
		}
		return list; 
	}

	public void addFile(String id, long size) {
		MetadataAttr attr = getOrCreateMetadataAttr(id);
		attr.setTotalSize(attr.getTotalSize() + size);
		attr.setFilesCount(attr.getFilesCount() + 1);
		tb.put(id, attr);
	}

	/*
	 * size -,+(size>0 or size<0)
	 */
	public void updateFile(String id, long size) {
		MetadataAttr attr = getOrCreateMetadataAttr(id);
		attr.setTotalSize(attr.getTotalSize() + size);
		tb.put(id, attr);
	}

	public void deleteFile(String id, long size) {
		MetadataAttr attr = getOrCreateMetadataAttr(id);
		attr.setTotalSize(attr.getTotalSize() - Math.abs(size));
		attr.setFilesCount(attr.getFilesCount() - 1);
		tb.put(id, attr);
	}

	public void addEmptyFolder(String id) {
		MetadataAttr attr = getOrCreateMetadataAttr(id);
		attr.setFoldersCount(attr.getFoldersCount() + 1);
		tb.put(id, attr);
	}

	public void deleteEmptyFolder(String id) {
		MetadataAttr attr = getOrCreateMetadataAttr(id);
		attr.setFoldersCount(attr.getFoldersCount() - 1);
		tb.put(id, attr);
	}
	
	public void addFolder(CountAffected countAffected) {
		addFolder(countAffected.getParentId(), countAffected.getTotalSize(), countAffected.getFileCount(), countAffected.getFolderCount(),
				countAffected.getCollaborateCount(), countAffected.getSyncRootCount(), countAffected.getRefLinkCount());
	}

	public void addFolder(String id, long fileSize, int fileCount, int folderCount, int collaborateCount, int syncRootCount, int refLinkCount) {
		MetadataAttr attr = getOrCreateMetadataAttr(id);
		attr.setFoldersCount(attr.getFoldersCount() + folderCount);
		attr.setFilesCount(attr.getFilesCount() + fileCount);
		attr.setTotalSize(attr.getTotalSize() + fileSize);
		attr.setCollaborateCount(attr.getCollaborateCount() + collaborateCount);
		attr.setSyncRootCount(attr.getSyncRootCount() + syncRootCount);
		attr.setRefLinkCount(attr.getRefLinkCount() + refLinkCount);
		tb.put(id, attr);
	}
	
	public void deleteFolder(CountAffected countAffected) {
		deleteFolder(countAffected.getParentId(), countAffected.getTotalSize(), countAffected.getFileCount(), countAffected.getFolderCount(),
				countAffected.getCollaborateCount(), countAffected.getSyncRootCount(), countAffected.getRefLinkCount());
	}

	public void deleteFolder(String id, long fileSize, int fileCount, int folderCount, int collaborateCount, int syncRootCount, int refLinkCount) {
		MetadataAttr attr = getOrCreateMetadataAttr(id);
		attr.setFoldersCount(attr.getFoldersCount() - Math.abs(folderCount));
		attr.setFilesCount(attr.getFilesCount() - Math.abs(fileCount));
		attr.setTotalSize(attr.getTotalSize() - Math.abs(fileSize));
		attr.setCollaborateCount(attr.getCollaborateCount() - Math.abs(collaborateCount));
		attr.setSyncRootCount(attr.getSyncRootCount() - Math.abs(syncRootCount));
		attr.setRefLinkCount(attr.getRefLinkCount() - Math.abs(refLinkCount));
		tb.put(id, attr);
	}


}
