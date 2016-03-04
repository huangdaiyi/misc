package model;

import java.io.Serializable;

import utils.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import constants.CommonFolders;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PathInfo implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;

	private String userId = "";
	private String fullOriginPath = "";
	private String deviceUid = "";
	private String fullSourcePath = "";
	private String ownerId = "";
	private String fullOwnerPath = "";
	private String sharedRootId = "";
	private String syncRootId = "";

	private boolean underSharedFolders = false;
	private boolean isFolder = false;

	public PathInfo() {
		super();
	}

	public PathInfo(String userId, String fullOriginPath, String deviceUid, String fullSourcePath, String ownerId, String fullOwnerPath, String sharedRootId) {
		super();
		this.userId = userId;
		this.fullOriginPath = fullOriginPath;
		this.deviceUid = deviceUid;
		this.fullSourcePath = fullSourcePath;
		this.ownerId = ownerId;
		this.fullOwnerPath = fullOwnerPath;
		this.sharedRootId = sharedRootId;
	}

	public PathInfo(String userId, String ownerId, String fullOwnerPath, String sharedRootId, boolean isFolder) {
		super();
		this.userId = userId;
		this.ownerId = ownerId;
		this.fullOwnerPath = fullOwnerPath;
		this.sharedRootId = sharedRootId;
		this.isFolder = isFolder;
	}

	public PathInfo(String userId, String fullOwnerPath, boolean isFolder) {
		super();
		this.userId = userId;
		this.fullOwnerPath = fullOwnerPath;
		this.isFolder = isFolder;
	}
	
	public PathInfo(String userId, BaseMetadata metadata, String fullOriginPath) {
		super();
		this.userId = userId;
		this.fullOriginPath = fullOriginPath;
		this.deviceUid = metadata.getDeviceUid();
		this.fullSourcePath = metadata.getSourcePath();
		this.ownerId = metadata.getOwnerId();
		this.fullOwnerPath = fullOriginPath.toLowerCase();
		this.sharedRootId = metadata.getSharedRootId();
		this.syncRootId = metadata.getSyncRootId();
		this.underSharedFolders = !StringUtils.isNullOrEmpty(metadata.getSharedRootId());
		this.isFolder = metadata.isFolder();
	}

	public PathInfo clone() {
		try {
			return (PathInfo) super.clone();
		} catch (CloneNotSupportedException e) {
		}
		throw new RuntimeException();
	}

	public void setUnderSharedFolders(boolean isUnderSharedFolder) {
		this.underSharedFolders = isUnderSharedFolder;
	}

	public boolean isUnderMyDeviceSyncFolders() {
		return StringUtils.isNullOrEmpty(fullSourcePath) == false;
	}

	public boolean isUnderSharedFolders() {
		return this.underSharedFolders;
	}

	public boolean isUnderMyBackupData() {
		String lowerCaseOwnerPath = StringUtils.trimFilePathRoot(fullOwnerPath).toLowerCase();
		return lowerCaseOwnerPath.equals(CommonFolders.MY_BACKUP_DATA.toString()) || lowerCaseOwnerPath.startsWith(CommonFolders.MY_BACKUP_DATA.toString().concat("/"));
	}

	public boolean isUnderMyAppSyncFolders() {
		return this.getFullSourcePath().startsWith(this.userId.concat("/").concat(CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString()).concat("/"));
	}

	/**
	 * userId/my backup data/xxx -> userId/xxx
	 */
	public String generateBackupFullOwnerPath() {
		if (isUnderMyBackupData() == false) {
			return "";
		}
		return StringUtils.concatFilePath(ownerId, StringUtils.trimFilePathRoot(StringUtils.trimFilePathRoot(fullOwnerPath))).toLowerCase();
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getFullOriginPath() {
		return fullOriginPath;
	}

	public void setFullOriginPath(String fullOriginPath) {
		this.fullOriginPath = fullOriginPath;
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

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}

	public String getFullOwnerPath() {
		return fullOwnerPath;
	}

	public void setFullOwnerPath(String fullOwnerPath) {
		this.fullOwnerPath = fullOwnerPath;
	}

	public String getSharedRootId() {
		return sharedRootId;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}

	public boolean isFolder() {
		return isFolder;
	}

	public void setFolder(boolean isFolder) {
		this.isFolder = isFolder;
	}

	public String getSyncRootId() {
		return syncRootId;
	}

	public void setSyncRootId(String syncRootId) {
		this.syncRootId = syncRootId;
	}

}
