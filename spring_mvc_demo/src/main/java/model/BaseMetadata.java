package model;

import java.io.Serializable;

import utils.StringUtils;
import factory.PathFactory;

public class BaseMetadata implements Serializable, Cloneable {
	private static final long serialVersionUID = 1L;
	
	private String id = "";
	private String parentId = "";
	private String name = "";
	private boolean folder = false;
	private int sortPriority = 0;
	private long size = 0;
	private String originName = "";
	private long modifiedAt = 0;
	private String modifiedBy = "";
	private String deviceUid = "";
	private String fullSourcePath = "";
	private String note = "";
	private String params = "";
	private String iconBlockId = "";
	private String iconText = "";
	private String iconTextColor = "";
	private String iconTextStyle = "";
	private boolean encrypted = false;
	private long createdAt = 0;
	private String createdBy = "";
	private String originId = "";
	private String blockId = "";
	private String type = null;
	private String ownerId = "";
	private boolean backup = false;
	private long totalSize = 0;
	private int foldersCount = 0;
	private int filesCount = 0;
	private int width = 0;
	private int height = 0;
	private String sharedRootId = "";
	private String syncRootId = "";
	@Deprecated
	private String path;
	private boolean visible = true;
	private int collaborateCount = 0;
	private int syncRootCount = 0;
	private int refLinkCount = 0;
	private String refId = "";
	private String refToOwnerId = "";
	private String itemTextColor;
	private String itemBgColor;
	private Boolean itemTextBold;
	private Boolean itemTextItalic;
	private Boolean itemTextUnderline;
	
	public BaseMetadata() {
		super();
	}

	public String generateDownloadUrl(String gatewayUrl) {
		return String.format("%s/file/%s", gatewayUrl, blockId);
	}

	public String generateStreamingBaseUrl(String gatewayUrl) {
		return String.format("%s/file/%s_streaming_", gatewayUrl, blockId);
	}

	public String generateReaderBaseUrl(String gatewayUrl) {
		return String.format("%s/file/%s_reader_", gatewayUrl, blockId);
	}

	public boolean isStreamingFile(PathFactory pathFactory) {
		return pathFactory.isStreamingFile(name);
	}

	public boolean isReaderFile(PathFactory pathFactory) {
		return pathFactory.isReaderFile(name);
	}

	public boolean isPicture(PathFactory pathFactory) {
		return pathFactory.isPicture(name);
	}

	public String getSourcePath() {
		if (fullSourcePath.isEmpty()) {
			return "";
		}
		return fullSourcePath.substring(fullSourcePath.indexOf("/") + 1);
	}

	public boolean hasNote() {
		return note != null && note.isEmpty() == false;
	}
	
	public boolean isSharedRoot() {
		return sharedRootId == id;
	}
	
	public boolean isSyncRoot() {
		return syncRootId == id;
	}
	
	public boolean isRefLink() {
		return !StringUtils.isNullOrEmpty(refId);
	}
	
	// NOT include the metadata itself
	public boolean hasCollaborate() {
		return isFolder() && collaborateCount > 0 && !isSharedRoot();
	}
	
	// NOT include the metadata itself
	public boolean hasSyncFolder() {
		return isFolder() && syncRootCount > 0 && !isSyncRoot();
	}
	
	// NOT include the metadata itself
	public boolean hasRefLink() {
		return isFolder() && refLinkCount > 0 && !isRefLink();
	}
	
	@Override
	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}

	public void fromBaseMetadata(BaseMetadata baseMetadata) {
		this.setId(baseMetadata.getId());
		this.setParentId(baseMetadata.getParentId());
		this.setName(baseMetadata.getName());
		this.setFolder(baseMetadata.isFolder());
		this.setSortPriority(baseMetadata.getSortPriority());
		this.setSize(baseMetadata.getSize());
		this.setOriginName(baseMetadata.getOriginName());
		this.setModifiedAt(baseMetadata.getModifiedAt());
		this.setModifiedBy(baseMetadata.getModifiedBy());
		this.setDeviceUid(baseMetadata.getDeviceUid());
		this.setFullSourcePath(baseMetadata.getFullSourcePath());
		this.setNote(baseMetadata.getNote());
		this.setParams(baseMetadata.getParams());
		this.setIconBlockId(baseMetadata.getIconBlockId());
		this.setIconText(baseMetadata.getIconText());
		this.setIconTextColor(baseMetadata.getIconTextColor());
		this.setIconTextStyle(baseMetadata.getIconTextStyle());
		this.setEncrypted(baseMetadata.isEncrypted());
		this.setCreatedAt(baseMetadata.getCreatedAt());
		this.setCreatedBy(baseMetadata.getCreatedBy());
		this.setBlockId(baseMetadata.getBlockId());
		this.setType(baseMetadata.getType());
		this.setOwnerId(baseMetadata.getOwnerId());
		this.setBackup(baseMetadata.isBackup());
		this.setTotalSize(baseMetadata.getTotalSize());
		this.setFoldersCount(baseMetadata.getFoldersCount());
		this.setFilesCount(baseMetadata.getFilesCount());
		this.setWidth(baseMetadata.getWidth());
		this.setHeight(baseMetadata.getHeight());
		this.setSharedRootId(baseMetadata.getSharedRootId());
		this.setSyncRootId(baseMetadata.getSyncRootId());
		this.setPath(baseMetadata.getPath());
		this.setVisible(baseMetadata.isVisible());
		this.setCollaborateCount(baseMetadata.getCollaborateCount());
		this.setSyncRootCount(baseMetadata.getSyncRootCount());
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getParentId() {
		return parentId;
	}

	public void setParentId(String parentId) {
		this.parentId = parentId;
	}

	public String getName() {
		return name;
	}

	public String getIconText() {
		return iconText;
	}

	public void setIconText(String iconText) {
		this.iconText = iconText;
	}

	public String getIconTextColor() {
		return iconTextColor;
	}

	public void setIconTextColor(String iconTextColor) {
		this.iconTextColor = iconTextColor;
	}

	public String getIconTextStyle() {
		return iconTextStyle;
	}

	public void setIconTextStyle(String iconTextStyle) {
		this.iconTextStyle = iconTextStyle;
	}

	public void setName(String name) {
		this.name = name;
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

	public int getSortPriority() {
		return sortPriority;
	}

	public void setSortPriority(int sortPriority) {
		this.sortPriority = sortPriority;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	public String getOriginName() {
		return originName;
	}

	public void setOriginName(String originName) {
		this.originName = originName;
	}

	public long getModifiedAt() {
		return modifiedAt;
	}

	public void setModifiedAt(long modifiedAt) {
		this.modifiedAt = modifiedAt;
	}

	public String getModifiedBy() {
		return modifiedBy;
	}

	public void setModifiedBy(String modifiedBy) {
		this.modifiedBy = modifiedBy;
	}

	public String getDeviceUid() {
		return deviceUid;
	}

	public void setDeviceUid(String deviceUid) {
		this.deviceUid = deviceUid;
	}

	public String getFullSourcePath() {
		return fullSourcePath;
	}

	public void setFullSourcePath(String fullSourcePath) {
		this.fullSourcePath = fullSourcePath;
	}

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

	public String getParams() {
		return params;
	}

	public void setParams(String params) {
		this.params = params;
	}

	public String getIconBlockId() {
		return iconBlockId;
	}

	public void setIconBlockId(String iconBlockId) {
		this.iconBlockId = iconBlockId;
	}

	public boolean isEncrypted() {
		return encrypted;
	}

	public void setEncrypted(boolean encrypted) {
		this.encrypted = encrypted;
	}

	public long getCreatedAt() {
		return createdAt;
	}

	public void setCreatedAt(long createdAt) {
		this.createdAt = createdAt;
	}

	public String getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	public String getBlockId() {
		return blockId;
	}

	public void setBlockId(String blockId) {
		this.blockId = blockId;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}

	public boolean isBackup() {
		return backup;
	}

	public void setBackup(boolean backup) {
		this.backup = backup;
	}

	public long getTotalSize() {
		return totalSize;
	}

	public void setTotalSize(long totalSize) {
		this.totalSize = totalSize;
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

	public String getSharedRootId() {
		return sharedRootId;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}

	public String getSyncRootId() {
		return syncRootId;
	}

	public void setSyncRootId(String syncRootId) {
		this.syncRootId = syncRootId;
	}

	@Deprecated
	public String getPath() {
		return path;
	}

	@Deprecated
	public void setPath(String path) {
		this.path = path;
	}

	public boolean isVisible() {
		return visible;
	}

	public void setVisible(boolean visible) {
		this.visible = visible;
	}

	public String getOriginId() {
		return originId;
	}

	public void setOriginId(String originId) {
		this.originId = originId;
	}

	
	public String getRealName(){
		if (!this.isFolder() && this.isBackup() && this instanceof BackupMetadata) {
			return StringUtils.formatName(getName(), ((BackupMetadata)this).getBackupNo(), false);
		}
		return getName();
	}
	
	public String getRealOriginalName(){
		if (!this.isFolder() && this.isBackup() && this instanceof BackupMetadata) {
			return StringUtils.formatName(getOriginName(), ((BackupMetadata)this).getBackupNo(), false);
		}
		return getOriginName();
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

	public String getRefId() {
		return refId;
	}

	public void setRefId(String refId) {
		this.refId = refId;
	}

	public String getRefToOwnerId() {
		return refToOwnerId;
	}

	public void setRefToOwnerId(String refToOwnerId) {
		this.refToOwnerId = refToOwnerId;
	}

	public int getRefLinkCount() {
		return refLinkCount;
	}

	public void setRefLinkCount(int refLinkCount) {
		this.refLinkCount = refLinkCount;
	}

	public String getItemTextColor() {
		return itemTextColor;
	}

	public void setItemTextColor(String itemTextColor) {
		this.itemTextColor = itemTextColor;
	}

	public String getItemBgColor() {
		return itemBgColor;
	}

	public void setItemBgColor(String itemBgColor) {
		this.itemBgColor = itemBgColor;
	}

	public Boolean isItemTextBold() {
		return itemTextBold;
	}

	public void setItemTextBold(Boolean itemTextBold) {
		this.itemTextBold = itemTextBold;
	}

	public Boolean isItemTextItalic() {
		return itemTextItalic;
	}

	public void setItemTextItalic(Boolean itemTextItalic) {
		this.itemTextItalic = itemTextItalic;
	}

	public Boolean isItemTextUnderline() {
		return itemTextUnderline;
	}

	public void setItemTextUnderline(Boolean itemTextUnderline) {
		this.itemTextUnderline = itemTextUnderline;
	}
}
