package model.response;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import model.BackupMetadata;
import model.BaseMetadata;
import utils.StringUtils;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import constants.MetadataType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class FolderResponse implements Serializable {

	private static final long serialVersionUID = 1;

	private String id;
	private String name;
	@JsonProperty("device_name")
	private String deviceName;
	private String path;
	private MetadataType type;
	@JsonProperty("last_edit_time")
	private long modifiedAt;
	@JsonProperty("files_count")
	private int filesCount;
	@JsonProperty("folders_count")
	private int foldersCount;
	@JsonProperty("subfilecount")
	private int subFileCount;
	@JsonProperty("subfoldercount")
	private int subFolderCount;
	@JsonProperty("size")
	private long totalSize;
	@JsonProperty("priority")
	private int sortPriority;
	@JsonProperty("note")
	private boolean hasNote;
	private boolean independent;
	@JsonProperty("owner_id")
	private String ownerId;
	@JsonProperty("icon_block_id")
	private String iconBlockId;
	@JsonProperty("icon_text")
	private String iconText;
	@JsonProperty("icon_text_color")
	private String iconTextColor;
	@JsonProperty("icon_text_style")
	private String iconTextStyle;
	@JsonProperty("is_encrypted")
	private boolean encrypted;
	@JsonProperty("created_at")
	private long createdAt;
	@JsonProperty("shared_root_id")
	private String sharedRootId;
	@JsonProperty("sync_root_id")
	private String syncRootId;
	@JsonProperty("source_path")
	private String sourcePath;
	@JsonProperty("uploaded_by")
	private String createdBy;
	@JsonProperty("params")
	private Map<String, Object> params;
	@JsonProperty("members_count")
	private int membersCount;
	@JsonProperty("has_collaborate")
	private boolean hasCollaborate;
	@JsonProperty("has_sync_folder")
	private boolean hasSyncFolder;
	@JsonProperty("has_unread_file")
	private boolean hasUnreadFile;
	@JsonProperty("item_text_color")
	private String itemTextColor;
	@JsonProperty("item_bg_color")
	private String itemBgColor;
	@JsonProperty("item_text_bold")
	private Boolean itemTextBold;
	@JsonProperty("item_text_italic")
	private Boolean itemTextItalic;
	@JsonProperty("item_text_underline")
	private Boolean itemTextUnderline;

	private Map<String, Object> otherParams = new HashMap<String, Object>();

	public FolderResponse() {
		super();
	}

	public static FolderResponse parse(BaseMetadata metadata,
			String parentPath, Map<String, Object> appendOtherParams) {
		return parse(metadata, metadata.hasNote(), parentPath,
				appendOtherParams);
	}

	@SuppressWarnings("unchecked")
	public static FolderResponse parse(BaseMetadata metadata,
			boolean displayNote, String parentPath,
			Map<String, Object> appendOtherParams) {
		FolderResponse response = new FolderResponse();
		response.setId(metadata.getId());
		response.setName(metadata.getOriginName());
		response.setType(MetadataType.parse(metadata.getType()));
		response.setModifiedAt(metadata.getModifiedAt());
		response.setTotalSize(metadata.getTotalSize());
		response.setSortPriority(metadata.getSortPriority());
		response.setHasNote(metadata.hasNote());
		if (metadata instanceof BackupMetadata) {
			response.setIndependent(((BackupMetadata) metadata).isIndependent());
		}
		
		response.setIconBlockId(metadata.getIconBlockId());
		response.setEncrypted(metadata.isEncrypted());
		response.setCreatedAt(metadata.getCreatedAt());
		response.setPath(StringUtils.concatFilePath(parentPath,
				metadata.getOriginName()));
		if (StringUtils.isNullOrEmpty(metadata.getParams()) == false) {
			response.setParams(StringUtils.readJSON(metadata.getParams(),
					HashMap.class));
		}
		// other params
		if (appendOtherParams != null && appendOtherParams.isEmpty() == false) {
			response.getOtherParams().putAll(appendOtherParams);
		}
		if (displayNote) {
			response.getOtherParams().put("note_content", metadata.getNote());
		}
		response.setSharedRootId(metadata.getSharedRootId());
		response.setOwnerId(metadata.getOwnerId());
		response.setSyncRootId(metadata.getSyncRootId());
		response.setSourcePath(metadata.getSourcePath());
		response.setIconText(metadata.getIconText());
		response.setIconTextColor(metadata.getIconTextColor());
		response.setIconTextStyle(metadata.getIconTextStyle());
		response.setHasCollaborate(metadata.hasCollaborate() || metadata.hasRefLink());
		response.setHasSyncFolder(metadata.hasSyncFolder());
		response.setItemTextColor(metadata.getItemTextColor());
		response.setItemBgColor(metadata.getItemBgColor());
		response.setItemTextBold(metadata.isItemTextBold());
		response.setItemTextItalic(metadata.isItemTextItalic());
		response.setItemTextUnderline(metadata.isItemTextUnderline());
		return response;
	}

	@JsonAnyGetter
	public Map<String, Object> getOtherParams() {
		return otherParams;
	}

	@JsonAnyGetter
	public void setOtherParams(Map<String, Object> otherParams) {
		this.otherParams = otherParams;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDeviceName() {
		return deviceName;
	}

	public void setDeviceName(String deviceName) {
		this.deviceName = deviceName;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public MetadataType getType() {
		return type;
	}

	public void setType(MetadataType type) {
		this.type = type;
	}

	public String getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	public int getFilesCount() {
		filesCount = filesCount < 0 ? 0 : filesCount;
		return filesCount;
	}

	public void setFilesCount(int filesCount) {
		this.filesCount = filesCount;
	}

	public int getFoldersCount() {
		foldersCount = foldersCount < 0 ? 0 : foldersCount;
		return foldersCount;
	}

	public void setFoldersCount(int foldersCount) {
		this.foldersCount = foldersCount;
	}

	public long getTotalSize() {
		totalSize = totalSize < 0 ? 0 : totalSize;
		return totalSize;
	}

	public void setTotalSize(long totalSize) {
		this.totalSize = totalSize;
	}

	public int getSortPriority() {
		return sortPriority;
	}

	public void setSortPriority(int sortPriority) {
		this.sortPriority = sortPriority;
	}

	public boolean isHasNote() {
		return hasNote;
	}

	public void setHasNote(boolean hasNote) {
		this.hasNote = hasNote;
	}

	public boolean isIndependent() {
		return independent;
	}

	public void setIndependent(boolean independent) {
		this.independent = independent;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
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

	public String getSharedRootId() {
		return sharedRootId;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}

	public String getSourcePath() {
		return sourcePath;
	}

	public void setSourcePath(String sourcePath) {
		this.sourcePath = sourcePath;
	}

	public String getIconText() {
		return iconText == null ? "" : iconText;
	}

	public void setIconText(String iconText) {
		this.iconText = iconText;
	}

	public String getIconTextColor() {

		return iconTextColor == null ? "" : iconTextColor;
	}

	public void setIconTextColor(String iconTextColor) {
		this.iconTextColor = iconTextColor;
	}

	public String getIconTextStyle() {
		return iconTextStyle == null ? "" : iconTextStyle;
	}

	public void setIconTextStyle(String iconTextStyle) {
		this.iconTextStyle = iconTextStyle;
	}

	public String getSyncRootId() {
		return syncRootId;
	}

	public void setSyncRootId(String syncRootId) {
		this.syncRootId = syncRootId;
	}

	public int getSubFileCount() {
		return subFileCount;
	}

	public void setSubFileCount(int subFileCount) {
		this.subFileCount = subFileCount;
	}

	public int getSubFolderCount() {
		return subFolderCount;
	}

	public void setSubFolderCount(int subFolderCount) {
		this.subFolderCount = subFolderCount;
	}

	public Map<String, Object> getParams() {
		return params;
	}

	public void setParams(Map<String, Object> params) {
		this.params = params;
	}

	public int getMembersCount() {
		return membersCount;
	}

	public void setMembersCount(int membersCount) {
		this.membersCount = membersCount;
	}

	public long getModifiedAt() {
		return modifiedAt;
	}

	public void setModifiedAt(long modifiedAt) {
		this.modifiedAt = modifiedAt;
	}

	public boolean isHasCollaborate() {
		return hasCollaborate;
	}

	public void setHasCollaborate(boolean hasCollaborate) {
		this.hasCollaborate = hasCollaborate;
	}

	public boolean isHasSyncFolder() {
		return hasSyncFolder;
	}

	public void setHasSyncFolder(boolean hasSyncFolder) {
		this.hasSyncFolder = hasSyncFolder;
	}

	public boolean isHasUnreadFile() {
		return hasUnreadFile;
	}

	public void setHasUnreadFile(boolean hasUnreadFile) {
		this.hasUnreadFile = hasUnreadFile;
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
