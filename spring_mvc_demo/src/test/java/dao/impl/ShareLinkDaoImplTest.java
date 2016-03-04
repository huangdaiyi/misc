package dao.impl;

import model.ShareLink;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import utils.DateUtils;
import utils.StringUtils;
import base.BaseTestCase;
import dao.ShareLinkDao;

public class ShareLinkDaoImplTest extends BaseTestCase {

	@Autowired
	private ShareLinkDao shareLinkDao;

	
	@Test
	public void getShareLinkTest(){
		String metadataId = StringUtils.getUUID();
		ShareLink shareLink = createShareLink(metadataId);
		shareLinkDao.createShareLink(shareLink);
		String link = shareLinkDao.getShareLink(metadataId);
		
		shareLinkDao.deleteShareLink(metadataId);
		Assert.assertEquals(shareLink.getLink(), link);
	}
	
	/*
	 * @Test public void getShareLinkPasswordTest(){ String metadataId =
	 * StringUtils.getUUID(); ShareLink shareLink = createShareLink(metadataId);
	 * shareLinkDao.createShareLink(shareLink); String pwd =
	 * shareLinkDao.getShareLinkPassword(shareLink.getLink());
	 * 
	 * shareLinkDao.deleteShareLink(metadataId);
	 * Assert.assertEquals(shareLink.getPassword(), pwd); }
	 */

	@Test
	public void shareLinkTest() {
		String metadataId = StringUtils.getUUID();
		ShareLink shareLink = createShareLink(metadataId);
		shareLinkDao.createShareLink(shareLink);

		ShareLink shareLinkResult = shareLinkDao.getShareLinkInfo(shareLink
				.getLink());
		Assert.assertEquals(metadataId, shareLinkResult.getMetadataIndexId());
		shareLinkDao.deleteShareLink(metadataId);

		shareLinkResult = shareLinkDao.getShareLinkInfo(shareLink.getLink());
		Assert.assertEquals(null, shareLinkResult);
	}
	
	@Test
	public void updateShareLinkTest(){
		String metadataId = StringUtils.getUUID();
		ShareLink shareLink = createShareLink(metadataId);
		shareLinkDao.createShareLink(shareLink);
		
		shareLink.setBackup(true);
		shareLink.setExpired(123123123L);
		shareLink.setLastEditTime(DateUtils.nowUTCTimestamp());
		shareLink.setLastEditUser("last eidt user");
		shareLink.setPassword("modify123123");
		shareLink.setLastEditUser("test 2");
		
		ShareLink shareLinkResult = shareLinkDao.updateShareLink(shareLink);
		ShareLink updatedResult = shareLinkDao.getShareLinkInfo(shareLink.getLink());
		Assert.assertEquals(metadataId, shareLinkResult.getMetadataIndexId());
		
		shareLinkResult = shareLinkDao.getShareLinkInfo(shareLink.getLink());
		shareLinkDao.deleteShareLink(metadataId);
		Assert.assertEquals(shareLinkResult.getExpired(), updatedResult.getExpired());
		Assert.assertEquals(shareLinkResult.getLastEditUser(), updatedResult.getLastEditUser());
		Assert.assertEquals(shareLinkResult.getLastEditTime(), updatedResult.getLastEditTime());
		Assert.assertEquals(shareLinkResult.getPassword(), updatedResult.getPassword());
		Assert.assertEquals(shareLinkResult.getLastEditUser(), updatedResult.getLastEditUser());
	}

	private ShareLink createShareLink(String metadataIndexId) {

		ShareLink shareLink = new ShareLink();
		shareLink.setLink("123link");
		shareLink.setBackup(false);
		shareLink.setExpired(DateUtils.convertDatetime("2015-02-25 16:33:55"));
		shareLink.setLastEditTime(DateUtils.nowUTCTimestamp());
		shareLink.setExpired(DateUtils.convertDatetime("2015-03-25 16:33:55"));
		shareLink.setLastEditUser("user test");
		shareLink.setMetadataIndexId(metadataIndexId);
		shareLink.setPassword("123123");
		shareLink.setToken("test token");
		shareLink.setUser("test");
		return shareLink;

	}

}
