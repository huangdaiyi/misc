package dao.impl;

import model.ProfileProperty;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import base.BaseTestCase;
import dao.ProfilePropertyDao;

@ContextConfiguration("profile_property.xml")
public class ProfilePropertyDaoImplTest extends BaseTestCase {

	@Autowired
	private ProfileProperty profilePropertyToAdd;

	@Autowired
	private ProfilePropertyDao profilePropertyDao;

	@Before
	public void setup() {
		profilePropertyDao.replaceProfileProperty(profilePropertyToAdd);
	}

	@After
	public void after() {
		profilePropertyDao.deleteProfileAndProperties(profilePropertyToAdd.getUserId(), profilePropertyToAdd.getDeviceUid(), profilePropertyToAdd.getMetadataIndexId());
	}

	@Test
	public void testGetProfileId() {
		long profileId = profilePropertyDao.getProfileId(profilePropertyToAdd.getUserId(), profilePropertyToAdd.getDeviceUid(), profilePropertyToAdd.getMetadataIndexId());
		Assert.assertTrue(profileId > 0);
	}

	@Test
	public void testReplaceProfileProperty() {
		profilePropertyToAdd.setPropertyValue("10");
		profilePropertyToAdd = profilePropertyDao.replaceProfileProperty(profilePropertyToAdd);

		ProfileProperty profileProperty = profilePropertyDao.getProfileProperty(profilePropertyToAdd.getUserId(), profilePropertyToAdd.getDeviceUid(), profilePropertyToAdd.getMetadataIndexId(),
				profilePropertyToAdd.getPropertyName());
		Assert.assertEquals("10", profileProperty.getPropertyValue());
	}

	@Test
	public void testGetProfileProperty() {
		ProfileProperty profileProperty = profilePropertyDao.getProfileProperty(profilePropertyToAdd.getUserId(), profilePropertyToAdd.getDeviceUid(), profilePropertyToAdd.getMetadataIndexId(),
				profilePropertyToAdd.getPropertyName());
		Assert.assertEquals("5", profileProperty.getPropertyValue());
	}

	@Test
	public void testDeleteProfileAndProperties() {
		profilePropertyDao.deleteProfileAndProperties(profilePropertyToAdd.getUserId(), profilePropertyToAdd.getDeviceUid(), profilePropertyToAdd.getMetadataIndexId());

		ProfileProperty profileProperty = profilePropertyDao.getProfileProperty(profilePropertyToAdd.getUserId(), profilePropertyToAdd.getDeviceUid(), profilePropertyToAdd.getMetadataIndexId(),
				profilePropertyToAdd.getPropertyName());
		Assert.assertNull(profileProperty);
	}

}
