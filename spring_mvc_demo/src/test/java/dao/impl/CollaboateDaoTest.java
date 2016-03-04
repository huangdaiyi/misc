package dao.impl;

import model.CellPhone;
import model.CollaborateMember;
import model.request.CreateCollaborateRequest;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import utils.CellphonesUtils;
import base.BaseTestCase;
import dao.CollaborateDao;

@ContextConfiguration("collaborate.xml")
public class CollaboateDaoTest extends BaseTestCase {
	@Autowired
	private CollaborateDao collaborateDao;

	@Autowired
	private CreateCollaborateRequest createCollaborateRequest;

	@Autowired
	private CollaborateMember collaborateMember;

	private String metadataIndexId = "collaborate-metadata-index-id";

	private long memberId = 0;

	@Before
	public void steup() {
		collaborateDao.createCollaborate(metadataIndexId);
		this.memberId = collaborateDao.createCollaborateMember(
				collaborateMember, metadataIndexId);
	}

	@After
	public void after() {
		collaborateDao.deleteCollaborate4DB(metadataIndexId);
		collaborateDao.deleteCollaborateMember(this.memberId);
	}

	@Test
	public void testDeleteCollaborate() {
		collaborateDao.deleteCollaborate(metadataIndexId);
	}

	@Test
	public void testGetGollaborateMember() {
		CollaborateMember member = collaborateDao.getCollaborateMember(
				metadataIndexId, CellphonesUtils
						.composeCellphones(collaborateMember.getCellphones()));
		Assert.assertNotNull(member);
	}

	@Test
	public void testUpdateMemeberAccepted() {
		collaborateDao
				.updateMemberAccepted(true, metadataIndexId, CellphonesUtils
						.composeCellphones(collaborateMember.getCellphones()));
	}
	
	@Test
	public void testUpdateMemberPhoto(){
		String photoBlockId = "testID";
		int photoSize = 100;
		boolean isOK = collaborateDao.updateCollaborateMemberPhoto(this.memberId,photoBlockId,photoSize);
		Assert.assertTrue(isOK);
	}
	@Test
	public void testGetMemberByFragmentCellophone(){
		for (CellPhone cellPhone: collaborateMember.getCellphones()) {
			CollaborateMember member = collaborateDao.getCollaborateMemberByFragmentCellphone(metadataIndexId, cellPhone.toString());
			Assert.assertNotNull(member);
		}
	}
}
