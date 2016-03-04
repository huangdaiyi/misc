package dao.impl;

import java.util.ArrayList;
import java.util.List;

import model.Metadata;
import model.UserSetting;
import model.request.RestCommonRequest;
import model.request.RestUserRequest;
import model.response.GetUserResponse;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import service.UserService;
import utils.DateUtils;
import utils.StringUtils;
import base.BaseTestCase;
import dao.UserDao;

public class UserDaoImplTest extends BaseTestCase {
	@Autowired
	private UserDao userDaoImpl;

	@Autowired
	private UserService userService;

	final String id = StringUtils.getUUID();

	@Before
	public void initData() {
		GetUserResponse entity = new GetUserResponse();
		entity.setName(id);
		entity.setId(id);
		entity.setTotal(12356456);
		entity.setSettings("oldSetting");

		RestUserRequest request = new RestUserRequest();
		request.setToken("2dcea1f635d4358f70c9030edfeb3cc580d5de21");
		request.setUserId("0f55586ebdb9b3de820092f74aa5ba79");
		request.setDbHost("localhost");
		request.setDbInstance("sample Instance");
		request.setDbPort(80);

		boolean m = userService.reset(request);
		int n = userDaoImpl.createUser(entity);
		assert (n > 0 && m);
	}

	@Test
	public void GetUser() {
		GetUserResponse user = userDaoImpl.getUser(id);
		assert (user != null);
	}

	@Test
	public void updateSettingTest() {
		UserSetting setting = new UserSetting();
		setting.setUser(id);
		setting.setToken(id);
		setting.setSettings("unitTestSetting");
		setting.setInDate(DateUtils.nowUTCTimestamp());
		int n = userDaoImpl.updateSetting(setting);
		assert (n > 0);
	}

	@Test
	public void getUserAllNode() {
		int n = userDaoImpl.getUserAllNode("0f55586ebdb9b3de820092f74aa5ba78")
				.size();
		assert (n > 0);
	}

	@Test
	public void updateCollaborate() {
		List<String> list = new ArrayList<String>();
		list.add("0f55586ebdb9b3de820092f74aa5ba78");
		int n = userDaoImpl.updateCollaborate(list);
		assert (n > 0);
	}

	@Test
	public void deleteAllMobileFolders() {
		int n = userDaoImpl
				.deleteAllMobileFolders("0f55586ebdb9b3de820092f74aa5ba78");
		assert (n >= 0);
	}

	@Test
	public void deleteMobileFolders() {
		int n = userDaoImpl.deleteMobileFolders(
				"0f55586ebdb9b3de820092f74aa5ba78", "uuu");
		assert (n >= 0);
	}

	@Test
	public void getOrdinaryCommonFolders() {
		RestCommonRequest requestOne = new RestCommonRequest();
		RestCommonRequest requestTwo = new RestCommonRequest();
		requestOne.setToken("2dcea1f635d4358f70c9030edfeb3cc580d5de21");
		requestOne.setUserId("0f55586ebdb9b3de820092f74aa5ba78");
		requestOne.setDeviceUid("");
		requestOne.setDbHost("localhost");
		requestOne.setDbInstance("sample Instance");
		requestOne.setDbPort(80);

		requestTwo.setToken("2dcea1f635d4358f70c9030edfeb3cc580d5de21");
		requestTwo.setUserId("0f55586ebdb9b3de820092f74aa5ba78");
		requestTwo.setDeviceUid("uuu");
		requestTwo.setDbHost("localhost");
		requestTwo.setDbInstance("sample Instance");
		requestTwo.setDbPort(80);

		List<Metadata> listOne = userDaoImpl
				.getOridinaryCommonFolderChild(requestOne);
		List<Metadata> listTwo = userDaoImpl
				.getOridinaryCommonFolderChild(requestTwo);
		assert (listOne.size() >= listTwo.size());
	}

	@Test
	public void getAllCommonFiles() {
		RestCommonRequest request = new RestCommonRequest();
		request.setToken("2dcea1f635d4358f70c9030edfeb3cc580d5de21");
		request.setUserId("0f55586ebdb9b3de820092f74aa5ba78");
		request.setDbHost("localhost");
		request.setDbInstance("sample Instance");
		request.setDbPort(80);

		request.setPath("");
		request.setDeviceUid("");
		List<Metadata> list1 = userDaoImpl.getAllCommonFiles(request);

		request.setPath("");
		request.setDeviceUid("uuu");
		List<Metadata> list2 = userDaoImpl.getAllCommonFiles(request);

		request.setPath("My Document");
		request.setDeviceUid("uuu");
		List<Metadata> list3 = userDaoImpl.getAllCommonFiles(request);

		request.setPath("My Document");
		request.setDeviceUid("");
		List<Metadata> list4 = userDaoImpl.getAllCommonFiles(request);

		request.setPath("PDF");
		request.setDeviceUid("");
		List<Metadata> list5 = userDaoImpl.getAllCommonFiles(request);

		request.setPath("PDF");
		request.setDeviceUid("uuu");
		List<Metadata> list6 = userDaoImpl.getAllCommonFiles(request);
		int n = list1.size() * list2.size() * list3.size() * list4.size()
				* list5.size() * list6.size();
		assert (n >= 0);
	}

	@Test
	public void getMyDocumentFileChild() {
		RestCommonRequest request = new RestCommonRequest();
		request.setToken("2dcea1f635d4358f70c9030edfeb3cc580d5de21");
		request.setUserId("0f55586ebdb9b3de820092f74aa5ba78");
		request.setDbHost("localhost");
		request.setDbInstance("sample Instance");
		request.setDbPort(80);
		request.setPath("My Document");
		request.setDeviceUid("");
		List<Metadata> list1 = userDaoImpl.getMyDocumentFileChild(request);
		request.setPath("My Document");
		request.setDeviceUid("uuu");
		List<Metadata> list2 = userDaoImpl.getMyDocumentFileChild(request);
		assert (list1.size() >= list2.size());
	}

	@Test
	public void deleteBackupAttr() {
		int n = userDaoImpl
				.deleteBackupInAttr("0f55586ebdb9b3de820092f74aa5ba78");
		assert (n >= 0);
	}

	@Test
	public void deleteAllBackupAttr() {
		int n = userDaoImpl
				.deleteAllBackupData("0f55586ebdb9b3de820092f74aa5ba78");
		assert (n >= 0);
	}
}
