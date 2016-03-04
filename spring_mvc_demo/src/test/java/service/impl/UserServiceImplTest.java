package service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import model.ActiveHistory;
import model.UserSetting;
import model.request.RestCommonRequest;
import model.request.RestUserRequest;
import model.request.UpdateSettingsRequest;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import service.UserService;
import base.BaseTestCase;
import dao.impl.UserDaoImpl;

@ContextConfiguration("user.xml")
public class UserServiceImplTest extends BaseTestCase {

	@Autowired
	private UserService userService;

	@Autowired
	private UserServiceImpl userServiceImpl;

	@Autowired
	private DynamoDBServiceImpl dynamoDBServiceImpl;

	@Autowired
	private String token;

	@Autowired
	private String unique_id;

	@Mock
	private UserDaoImpl userDaoImpl;

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);
	}

	// @Test
	public void resetSettingtest() {
		UpdateSettingsRequest req = new UpdateSettingsRequest();
		req.setSettings("new setting");
		req.setUserId("fc1fd9e71de4f8c53bb7d2f1182772d3");
		int rst_mock = 0;
		UserSetting userSetting = new UserSetting();
		userSetting.setSettings(req.getSettings());
		userSetting.setUser(req.getUserId());
		Mockito.when(userDaoImpl.updateSetting(userSetting)).thenReturn(
				rst_mock = 1);
		boolean rst = userService.updateSettings(req);
		assert (rst && rst_mock > 1);
	}

	// @Test
	// public void resetCommon(){
	// RestCommonRequest req=new RestCommonRequest();
	// req.setUserId(userId)
	// }

	@Test
	public void resetUsertest() {
		RestUserRequest req = new RestUserRequest();
		req.setDbHost("DbHost");
		req.setDbInstance("DbInstance");
		req.setDbPort(80);
		req.setDeviceId("DeviceId");
		req.setToken("2dcea1f635d4358f70c9030edfeb3cc580d5de21");
		req.setUserId("0f55586ebdb9b3de820092f74aa5ba78");
		userService.reset(req);
	}

	// @Test
	public void restCommontest() {
		RestCommonRequest request = new RestCommonRequest();
		request.setToken("2dcea1f635d4358f70c9030edfeb3cc580d5de21");
		request.setUserId("0f55586ebdb9b3de820092f74aa5ba78");
		request.setPath("");
		request.setDeviceUid("");
		userService.resetCommon(request);
	}

	// @Test
	public void activity_HistoryTest() {
		List<ActiveHistory> list = new ArrayList<ActiveHistory>();
		ActiveHistory history1 = new ActiveHistory();
		ActiveHistory history2 = new ActiveHistory();
		ActiveHistory history3 = new ActiveHistory();

		history1.setId(UUID.randomUUID().toString());
		history1.setPath("Root/My document");
		history1.setOldPath("Root/Mydocument/excel");
		history1.setOldPath("Tristan");
//		history1.setOwner("Tristan1");
		history1.setMetadataId(UUID.randomUUID().toString());
		history1.setLastEditUser("Tistan");
		history1.setLastEditTime("150230132456");
//		history1.setChangeType(SystemEvent.DEVICE_LOGOUT);
//		history1.setEditToken("asdfas-lxkajie-asdfjasl");
//		history1.setTokens("asdfas-lxkajie-asdfjasl");
//		history1.setLevel(ActivityHistoryLevel.NONE);

		history2.setId(UUID.randomUUID().toString());
		history2.setPath("Root/My document");
		history2.setOldPath("Root/Mydocument/excel");
		history2.setOldPath("Tristan");
//		history2.setOwner("Tristan2");
		history2.setMetadataId(UUID.randomUUID().toString());
		history2.setLastEditUser("Tistan");
		history2.setLastEditTime("150230132456");
//		history2.setChangeType(SystemEvent.DEVICE_LOGOUT);
//		history2.setEditToken("asdfas-lxkajie-asdfjasl");
//		history2.setTokens("asdfas-lxkajie-asdfjasl");
//		history2.setLevel(ActivityHistoryLevel.NONE);

		history3.setId(UUID.randomUUID().toString());
		history3.setPath("Root/My document");
		history3.setOldPath("Root/Mydocument/excel");
		history3.setOldPath("Tristan");
//		history3.setOwner("Tristan3");
		history3.setMetadataId(UUID.randomUUID().toString());
		history3.setLastEditUser("Tistan");
		history3.setLastEditTime("150230132456");
//		history3.setChangeType(SystemEvent.DEVICE_LOGOUT);
//		history3.setEditToken("asdfas-lxkajie-asdfjasl");
//		history3.setTokens("asdfas-lxkajie-asdfjasl");
//		history3.setLevel(ActivityHistoryLevel.NONE);

		list.add(history1);
		list.add(history2);
		list.add(history3);
		dynamoDBServiceImpl.createActivityHistory(list);
	}
}
