package model;

import com.fasterxml.jackson.annotation.JsonProperty;

import constants.SystemEvent;

public class XmppSendInfo {

	@JsonProperty("owner")
	private String ownerId;
	@JsonProperty("change_type")
	private SystemEvent systemEvent;
	@JsonProperty("last_edit_user")
	private String lastEditUser;
	@JsonProperty("last_edit_time")
	private String lastEditTime;
	@JsonProperty("path")
	private String path;
	@JsonProperty("shared_root_id")
	private String sharedRootId;
	@JsonProperty("sync_root_id")
	private String syncRootId;
	@JsonProperty("old_path")
	private String oldPath;
	@JsonProperty("shared_folder_path")
	private String sharedFolderPath;
	@JsonProperty("source_path")
	private String sourcePath;
	@JsonProperty("old_source_path")
	private String oldSourcePath;
	@JsonProperty("old_owner")
	private String oldOwnerId;
	@JsonProperty("old_shared_root_id")
	private String oldSharedRootId;

	public XmppSendInfo() {
		super();
	}

	public XmppSendInfo(String ownerId, SystemEvent systemEvent) {
		super();
		this.ownerId = ownerId;
		this.systemEvent = systemEvent;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}

	public SystemEvent getSystemEvent() {
		return systemEvent;
	}

	public void setSystemEvent(SystemEvent systemEvent) {
		this.systemEvent = systemEvent;
	}

	public String getLastEditUser() {
		return lastEditUser;
	}

	public void setLastEditUser(String lastEditUser) {
		this.lastEditUser = lastEditUser;
	}

	public String getLastEditTime() {
		return lastEditTime;
	}

	public void setLastEditTime(String lastEditTime) {
		this.lastEditTime = lastEditTime;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getOldPath() {
		return oldPath;
	}

	public void setOldPath(String oldPath) {
		this.oldPath = oldPath;
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

	public String getSharedFolderPath() {
		return sharedFolderPath;
	}

	public void setSharedFolderPath(String sharedFolderPath) {
		this.sharedFolderPath = sharedFolderPath;
	}

	public String getSourcePath() {
		return sourcePath;
	}

	public void setSourcePath(String sourcePath) {
		this.sourcePath = sourcePath;
	}

	public String getOldSourcePath() {
		return oldSourcePath;
	}

	public void setOldSourcePath(String oldSourcePath) {
		this.oldSourcePath = oldSourcePath;
	}

	public String getOldOwnerId() {
		return oldOwnerId;
	}

	public void setOldOwnerId(String oldOwnerId) {
		this.oldOwnerId = oldOwnerId;
	}

	public String getOldSharedRootId() {
		return oldSharedRootId;
	}

	public void setOldSharedRootId(String oldSharedRootId) {
		this.oldSharedRootId = oldSharedRootId;
	}

}
