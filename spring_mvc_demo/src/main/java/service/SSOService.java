package service;

import java.util.List;
import java.util.Map;

import model.CellPhone;
import model.Device;
import model.RequestBase;
import model.request.GetSMSStatusRequest;
import model.request.SSOFindUserIdByCellphoneReq;
import model.request.SendSMSRequest;
import model.response.GetSMSResponse;
import model.response.SSODevicesInfo;
import model.response.SSOFindUserIdByCellphone;
import model.response.SSOUserInfoResponse;
import model.response.SendSMSResponse;

public interface SSOService extends UserAuthorizationService {

	public SSOUserInfoResponse getUserInfoByToken(RequestBase request);
	
	public SSODevicesInfo getDevicesByToken(RequestBase requestBase);
	
	public SSOFindUserIdByCellphone findUserIdByCellophones(SSOFindUserIdByCellphoneReq findUserIdByCellphone);
	
	public Map<String, String> findCellphoneUserIdMap(List<CellPhone> cellphones);

	@Deprecated
	public List<CellPhone> findCellphonesByUserId(String userId);
	
	public String findDisplayNameByUserId(String userId);
	
	public List<String> findDisplayNamesByUserIdList(List<String> userIdList);
	
	public Map<String, String> findUserIdDisplayNameMap(List<String> userIdList);

	public Device findDeviceNameStatusByDeviceId(String deviceId);
	
	public SendSMSResponse sendSMS(SendSMSRequest sendSMSRequest);
	
	public GetSMSResponse getSMSStatus(GetSMSStatusRequest getSMSStatus);

}
