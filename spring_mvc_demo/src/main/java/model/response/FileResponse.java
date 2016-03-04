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
public class FileResponse implements Serializable {

	private static final long serialVersionUID = 1;

	private String id;
	private String name;
	@JsonProperty("unique_id")
	private String deviceUid;
	@JsonProperty("device_name")
	private String deviceName;
	private String path;
	@JsonProperty("source_path")
	private String sourcePath;
	private MetadataType type;
	@JsonProperty("last_edit_time")
	private long modifiedAt;
	@JsonProperty("size")
	private long totalSize;
	@JsonProperty("priority")
	private int sortPriority;
	@JsonProperty("note")
	private boolean hasNote;
	private boolean independent;
	@JsonProperty("owner_id")
	private String ownerId;
	@JsonProperty("streaming_file_baseurl")
	private String streamingFileBaseUrl;
	@JsonProperty("reader_file_baseurl")
	private String readerFileBaseUrl;
	@JsonProperty("block_id")
	private String blockId;
	@JsonProperty("is_encrypted")
	private boolean encrypted;
	@JsonProperty("created_at")
	private long createdAt;
	@JsonProperty("uploaded_by")
	private String createdBy;
	@JsonProperty("icon_block_id")
	private String iconBlockId;
	@JsonProperty("icon_text")
	private String iconText = "";
	@JsonProperty("icon_text_color")
	private String iconTextColor = "";
	@JsonProperty("icon_text_style")
	private String iconTextStyle = "";
	@JsonProperty("shared_root_id")
	private String sharedRootId;
	@JsonProperty("sync_root_id")
	private String syncRootId;
	@JsonProperty("params")
	private Map<String, Object> params;

	private Map<String, Object> otherParams = new HashMap<String, Object>();
	@JsonProperty("already_read")
	private boolean alreadyRead;
	@JsonProperty("already_read_count")
	private int alreadyReadCount;
	
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

	public FileResponse() {
		super();
	}

	public static FileResponse parse(BaseMetadata metadata, String gatewayUrl,
			String parentPath, Map<String, Object> appendOtherParams) {
		return parse(metadata, metadata.hasNote(), gatewayUrl, parentPath,
				appendOtherParams);
	}

	@SuppressWarnings("unchecked")
	public static FileResponse parse(BaseMetadata metadata,
			boolean displayNote, String gatewayUrl, String parentPath,
			Map<String, Object> appendOtherParams) {
		if (metadata.isFolder()) {
			return null;
		}
		FileResponse response = new FileResponse();
		response.setId(metadata.getId());
		response.setName(metadata.getOriginName());
		response.setSourcePath(metadata.getSourcePath());
		response.setDeviceUid(metadata.getDeviceUid());
		response.setType(MetadataType.parse(metadata.getType()));
		response.setModifiedAt(metadata.getModifiedAt());
		response.setTotalSize(metadata.getSize());
		response.setSortPriority(metadata.getSortPriority());
		response.setHasNote(metadata.hasNote());
		if (metadata instanceof BackupMetadata) {
			BackupMetadata tempBackup = (BackupMetadata) metadata;
			response.setIndependent(tempBackup.isIndependent());
			response.setName(StringUtils.formatName(tempBackup.getOriginName(),
					tempBackup.getBackupNo(), false));
		}
		response.setOwnerId(metadata.getOwnerId());
		response.setStreamingFileBaseUrl(metadata
				.generateStreamingBaseUrl(gatewayUrl));
		response.setReaderFileBaseUrl(metadata
				.generateReaderBaseUrl(gatewayUrl));
		response.setBlockId(metadata.getBlockId());
		response.setEncrypted(metadata.isEncrypted());
		response.setCreatedAt(metadata.getCreatedAt());
		response.setIconBlockId(metadata.getIconBlockId());
		response.setPath(StringUtils.concatFilePath(parentPath,
				response.getName()));

		if (StringUtils.isNullOrEmpty(metadata.getParams()) == false) {
			response.setParams(StringUtils.readJSON(metadata.getParams(),
					HashMap.class));
		}
		Map<String, Object> params = response.getParams();
		if (metadata.getWidth() != 0 && metadata.getHeight() != 0) {
			if (params == null) {
				params = new HashMap<String, Object>();
			}
			if (params.get("file_type") == null) {
				params.put("file_type", "Picture");
			}
			params.put("img_height", String.valueOf(metadata.getHeight()));
			params.put("img_width", String.valueOf(metadata.getWidth()));
		}

		if (params != null) {
			response.setParams(params);
		}

		// other params
		if (appendOtherParams != null && appendOtherParams.isEmpty() == false) {
			response.getOtherParams().putAll(appendOtherParams);
		}
		if (displayNote) {
			response.getOtherParams().put("note_content", metadata.getNote());
		}
		;
		response.setSharedRootId(metadata.getSharedRootId());
		response.setSyncRootId(metadata.getSyncRootId());
		response.setIconText(metadata.getIconText());
		response.setIconTextColor(metadata.getIconTextColor());
		response.setIconTextStyle(metadata.getIconTextStyle());
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

	@JsonProperty("leave_by_unlink")
	public boolean isLeaveByUnlink() {
		return !StringUtils.isNullOrEmpty(sourcePath)
				&& type == MetadataType.NORMAL;
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

	public String getDeviceUid() {
		return deviceUid;
	}

	public void setDeviceUid(String deviceUid) {
		this.deviceUid = deviceUid;
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

	public String getSourcePath() {
		return sourcePath;
	}

	public void setSourcePath(String sourcePath) {
		this.sourcePath = sourcePath;
	}

	public MetadataType getType() {
		return type;
	}

	public void setType(MetadataType type) {
		this.type = type;
	}

	public long getModifiedAt() {
		return modifiedAt;
	}

	public void setModifiedAt(long modifiedAt) {
		this.modifiedAt = modifiedAt;
	}

	public String getCreatedBy() {
		return createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
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

	public String getStreamingFileBaseUrl() {
		return streamingFileBaseUrl;
	}

	public void setStreamingFileBaseUrl(String streamingFileBaseUrl) {
		this.streamingFileBaseUrl = streamingFileBaseUrl;
	}

	public String getReaderFileBaseUrl() {
		return readerFileBaseUrl;
	}

	public void setReaderFileBaseUrl(String readerFileBaseUrl) {
		this.readerFileBaseUrl = readerFileBaseUrl;
	}

	public String getBlockId() {
		return blockId;
	}

	public void setBlockId(String blockId) {
		this.blockId = blockId;
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

	public String getIconBlockId() {
		return iconBlockId;
	}

	public void setIconBlockId(String iconBlockId) {
		this.iconBlockId = iconBlockId;
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

	public Map<String, Object> getParams() {
		return params;
	}

	public void setParams(Map<String, Object> params) {
		this.params = params;
	}

	public String getSharedRootId() {
		return sharedRootId;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}

	public boolean isAlreadyRead() {
		return alreadyRead;
	}

	public void setAlreadyRead(boolean alreadyRead) {
		this.alreadyRead = alreadyRead;
	}

	public int getAlreadyReadCount() {
		return alreadyReadCount;
	}

	public void setAlreadyReadCount(int alreadyReadCount) {
		this.alreadyReadCount = alreadyReadCount;
	}

	public String getSyncRootId() {
		return syncRootId;
	}

	public void setSyncRootId(String syncRootId) {
		this.syncRootId = syncRootId;
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
