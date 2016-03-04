package service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import model.CellPhone;
import model.Device;
import model.RequestBase;
import model.request.GetSMSStatusRequest;
import model.request.SSOFindCellphonesByUserIdsRequest;
import model.request.SSOFindDeviceNameStatusByDeviceIdRequest;
import model.request.SSOFindDisplayNameByUserIdRequest;
import model.request.SSOFindUserIdByCellphoneReq;
import model.request.SSOUserInfoRequest;
import model.request.SendSMSRequest;
import model.response.GetSMSResponse;
import model.response.SSODevicesInfo;
import model.response.SSOFindCellphonesByUserIdsResponse;
import model.response.SSOFindDeviceNameStatusByDeviceIdResponse;
import model.response.SSOFindDisplayNameByUserIdResponse;
import model.response.SSOFindUserIdByCellphone;
import model.response.SSOUserInfoResponse;
import model.response.SendSMSResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;

import service.RestService;
import service.SSOService;
import utils.StringUtils;

@Service
public class SSOServiceImpl implements SSOService {

	@Value("${sso.host}")
	private String host;
	@Value("${sso.user_info}")
	private String userInfo;
	@Value("${sso.findCellphones}")
	private String findCellphones;
	@Value("${sso.findDisplayName}")
	private String findDisplayName;
	@Value("${sso.findDeviceName}")
	private String findDeviceName;
	@Value("${sso.servcieName}")
	private String servcieName;
	@Autowired
	private RestService restService;

	@Override
	public SSOUserInfoResponse getUserInfoByToken(RequestBase request) {
		String response = restService.sendPostRequestForObj(host
				+ userInfo, addHttpHeaders(), request, String.class);
		// SSO response contentType:"text/html"
		return StringUtils.readJSON(response, SSOUserInfoResponse.class);
	}

	@Override
	public RequestBase checkToken(String token) {
		SSOUserInfoRequest ssoUserInfoRequest = new SSOUserInfoRequest();
		ssoUserInfoRequest.setToken(token);
		SSOUserInfoResponse ssoUserInfo = getUserInfoByToken(ssoUserInfoRequest);
		if (ssoUserInfo == null) {
			return null;
		}
		return ssoUserInfo.toRequestBase(token);
	}

	@Override
	public SSODevicesInfo getDevicesByToken(RequestBase request) {
		String response = restService.sendPostRequestForObj(host
				+ "/devices/info", addHttpHeaders(), request, String.class);
		// SSO response contentType:"text/html"
		return StringUtils.readJSON(response, SSODevicesInfo.class);
	}

	private HttpHeaders addHttpHeaders() {
		HttpHeaders headers = new HttpHeaders();
		headers.add("ServiceName", this.servcieName);
		return headers;
	}

	@Override
	public SSOFindUserIdByCellphone findUserIdByCellophones(
			SSOFindUserIdByCellphoneReq findUserIdByCellphoneReq) {
		String response = restService.sendPostRequestForObj(host
				+ "/user/find_userId", addHttpHeaders(), findUserIdByCellphoneReq, String.class);
		return StringUtils.readJSON(response, SSOFindUserIdByCellphone.class);
	}
	
	@Override
	public Map<String, String> findCellphoneUserIdMap(List<CellPhone> cellphones) {
		Map<String, String> result = new HashMap<String, String>();
		
		SSOFindUserIdByCellphone foundUsers = findUserIdByCellophones(new SSOFindUserIdByCellphoneReq(cellphones));
		for(CellPhone cellphone : foundUsers.getUsers()) {
			if(!StringUtils.isNullOrEmpty(cellphone.getUserId())) {
				result.put(cellphone.toString(), cellphone.getUserId());
			}
		}
		
		return result;
	}

	@Deprecated
	@Override
	public List<CellPhone> findCellphonesByUserId(String userId) {
		SSOFindCellphonesByUserIdsRequest request = new SSOFindCellphonesByUserIdsRequest();
		List<String> userIds = new ArrayList<String>();
		userIds.add(userId);
		request.setUserIds(userIds);
		String response = restService.sendPostRequestForObj(host
				+ findCellphones, addHttpHeaders(), request, String.class);
		return StringUtils.readJSON(response, SSOFindCellphonesByUserIdsResponse.class).getUsers().get(0).getCellphones();
	}

	@Override
	public String findDisplayNameByUserId(String userId) {
		String displayName = "";
		if (userId != null && userId.isEmpty() == false) {
			List<String> result = findDisplayNamesByUserIdList(Arrays.asList(userId));
			if (result.isEmpty() == false && (displayName = result.get(0)) == null) {
				displayName = "";
			}
		}
		return displayName;
	}

	@Override
	public List<String> findDisplayNamesByUserIdList(List<String> userIdList) {
		SSOFindDisplayNameByUserIdRequest request = new SSOFindDisplayNameByUserIdRequest();
		request.setUsers(userIdList);
		String response = restService.sendPostRequestForObj(host + findDisplayName, addHttpHeaders(), request, String.class);
		try {
			return StringUtils.readJSON(response, SSOFindDisplayNameByUserIdResponse.class).getUserinfo();
		} catch (Exception e) {
		}
		return new ArrayList<String>();
	}
	
	@Override
	public Map<String, String> findUserIdDisplayNameMap(List<String> userIdList) {
		Map<String, String> result = new HashMap<String, String>();
		
		List<String> displayNames = findDisplayNamesByUserIdList(userIdList);
		for(int i = 0; i < userIdList.size(); i++) {
			if(displayNames.get(i) != null) {
				result.put(userIdList.get(i), displayNames.get(i));
			}
		}
		
		return result;
	}

	@Override
	public Device findDeviceNameStatusByDeviceId(String deviceId) {
		SSOFindDeviceNameStatusByDeviceIdRequest request = new SSOFindDeviceNameStatusByDeviceIdRequest();
		request.setDeviceId(deviceId);
		String response = restService.sendPostRequestForObj(host
				+ findDeviceName, addHttpHeaders(), request, String.class);
		SSOFindDeviceNameStatusByDeviceIdResponse responseObj = StringUtils.readJSON(response, SSOFindDeviceNameStatusByDeviceIdResponse.class);
		Device device = new Device();
		device.setDeviceId(deviceId);
		device.setName(responseObj.getDeviceName());
		device.setStatus(responseObj.getDeviceStatus());
		return device;
	}
	
	@Override
	public SendSMSResponse sendSMS(SendSMSRequest request) {
		String response = restService.sendPostRequestForObj(host
				+ "/service/send_sms", addHttpHeaders(), request, String.class);
		// SSO response contentType:"text/html"
		return StringUtils.readJSON(response, SendSMSResponse.class);
	}
	
	@Override
	public GetSMSResponse getSMSStatus(GetSMSStatusRequest request) {
		String response = restService.sendPostRequestForObj(host
				+ "/service/get_sms_status", addHttpHeaders(), request, String.class);
		// SSO response contentType:"text/html"
		return StringUtils.readJSON(response, GetSMSResponse.class);
	} 

}
