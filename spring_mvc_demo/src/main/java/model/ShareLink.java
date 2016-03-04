package model;

import java.io.Serializable;

import model.response.GetShareLinkResponse;
import utils.DateUtils;

public class ShareLink implements Serializable {
	private static final long serialVersionUID = 1L;

	private String metadataIndexId;
	private boolean backup;
	private String link;
	private String user;
	private String password;
	private String token;
	private Long expired;
	private String lastEditUser;
	private long lastEditTime;
	private boolean isFolder;

	public ShareLink() {
		super();
	}

	public GetShareLinkResponse toGetShareLinkResponse(BaseMetadata metadata, String path, boolean isDetail) {
		GetShareLinkResponse response = new GetShareLinkResponse();
		if (isDetail) {
			response.setBlockId(metadata.getBlockId());
		}
		if (this.getExpired() != null) {
			response.setExpired(DateUtils.convertDatetime(this.getExpired()));
		}
		response.setFolder(metadata.isFolder());
		response.setMetadataId(this.getMetadataIndexId());
		response.setModifiedAt(String.valueOf(this.getLastEditTime()));
		response.setModifiedBy(this.getLastEditUser());
		response.setPassword(this.getPassword());
		response.setOriginName(metadata.getOriginName());
		response.setSize(metadata.getTotalSize());
		response.setSourcePath(metadata.getSourcePath());
		response.setPath(path);
		response.setUser(metadata.getOwnerId());
		response.setToken(this.getToken());
		return response;
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

	public String getLink() {
		return link;
	}

	public void setLink(String link) {
		this.link = link;
	}

	public String getUser() {
		return user;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public Long getExpired() {
		return expired;
	}

	public void setExpired(Long expired) {
		this.expired = expired;
	}

	public String getLastEditUser() {
		return lastEditUser;
	}

	public void setLastEditUser(String lastEditUser) {
		this.lastEditUser = lastEditUser;
	}

	public long getLastEditTime() {
		return lastEditTime;
	}

	public void setLastEditTime(long lastEditTime) {
		this.lastEditTime = lastEditTime;
	}

	public boolean isFolder() {
		return isFolder;
	}

	public void setFolder(boolean isFolder) {
		this.isFolder = isFolder;
	}
	

}
