package dao;

import model.ShareLink;

public interface ShareLinkDao {

	public ShareLink createShareLink(ShareLink shareLink);

	public String getShareLink(String metadataIndexId);

	public ShareLink getShareLinkInfo(String link);

	public void deleteShareLink(String metadataIndexId);

	public ShareLink updateShareLink(ShareLink shareLink);

}
