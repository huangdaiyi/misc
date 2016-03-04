package service;

import model.PathInfo;
import constants.SystemEvent;

public interface XmppService {

	public void sendMessage(String userId, String deviceId, String messageContent);

	public void sendToAllDevices(SystemEvent systemEvent, String userId);

	public void sendToAllDevices(SystemEvent systemEvent, PathInfo pathInfo, PathInfo newPathInfo, boolean sendToAllMember, boolean sendToAcceptedMemberOnly);

	public void sendToAllDevices(SystemEvent systemEvent, PathInfo pathInfo, PathInfo newPathInfo);
	
	public void sendToAllDevices(SystemEvent systemEvent, PathInfo pathInfo);
}
