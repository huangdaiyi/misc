package service.impl;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.annotation.PostConstruct;


import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.ChatManager;
import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode;
import org.jivesoftware.smack.MessageListener;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.tcp.XMPPTCPConnection;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import constants.CommonFolders;
import constants.SystemEvent;
import dao.CollaborateDao;
import factory.MetadataFactory;
import factory.impl.ThreadPool;
import model.CollaborateMember;
import model.PathInfo;
import model.XmppSendInfo;
import service.SSOService;
import service.XmppService;
import utils.DateUtils;
import utils.StringUtils;

@Service
public class XmppServiceImpl implements XmppService {
	private static final Logger logger = LogManager.getLogger(XmppService.class);

	@Value("${xmpp.host}")
	private String xmppHost;
	@Value("${xmpp.port}")
	private Integer xmppPort;
	@Value("${xmpp.pwd}")
	private String xmppPWD;
	@Value("${xmpp.appName}")
	private String xmppAppName;

	@Autowired
	private CollaborateDao collaborateDao;
	@Autowired
	private SSOService ssoService;
	@Autowired
	private MetadataFactory metadataFactory;
	@Autowired
	private ThreadPool threadPool;

	private ConnectionConfiguration config;

	@PostConstruct
	private void init() {
		if (StringUtils.isNullOrEmpty(xmppHost) == false) {
			config = new ConnectionConfiguration(xmppHost, xmppPort);
			config.setReconnectionAllowed(true);
			config.setSendPresence(true);
			config.setSecurityMode(SecurityMode.disabled);
			config.setCompressionEnabled(false);
		}
	}

	@Override
	public void sendMessage(final String userId, final String toDeviceId, final String messageContent) {
		XMPPTCPConnection connection = connectAndLogin(userId, xmppPWD, generateResourceId(toDeviceId, xmppAppName));
		if (connection != null) {
			ChatManager chatManager = ChatManager.getInstanceFor(connection);
			Chat chat = chatManager.createChat(generateJid(userId), new MessageListener() {
				@Override
				public void processMessage(Chat chat, Message message) {
				}
			});
			try {
				chat.sendMessage(messageContent);
			} catch (Exception e) {
				logger.warn("xmpp send message error", e);
			} finally {
				try {
					Thread.sleep(50);
					connection.disconnect();
				} catch (Exception e) {
				}
			}
		}
	}

	@Override
	public void sendToAllDevices(SystemEvent systemEvent, String userId) {
		XmppSendInfo sendInfo = new XmppSendInfo();
		sendInfo.setOwnerId(userId);
		sendInfo.setSystemEvent(systemEvent);
		sendInfo.setLastEditUser(userId);
		sendInfo.setLastEditTime(String.valueOf(DateUtils.nowUTCTimestamp()));
		sendMessage(userId, "all", StringUtils.writeJSON(sendInfo));
	}

	@Override
	public void sendToAllDevices(SystemEvent systemEvent, PathInfo pathInfo) {
		sendToAllDevices(systemEvent, pathInfo, null);
	}
	
	@Override
	public void sendToAllDevices(SystemEvent systemEvent, PathInfo pathInfo, PathInfo newPathInfo) {
		sendToAllDevices(systemEvent, pathInfo, newPathInfo, true, true);
	}

	@Override
	public void sendToAllDevices(final SystemEvent systemEvent, final PathInfo pathInfo, final PathInfo newPathInfo, final boolean sendToAllMember, final boolean sendToAcceptedMemberOnly) {
		Thread thread = new Thread() {
			@Override
			public void run() {
				String path = StringUtils.trimFilePathRoot(pathInfo.getFullOwnerPath());
				String sourcePath = StringUtils.trimFilePathRoot(pathInfo.getFullSourcePath());
				String ownerId = pathInfo.getOwnerId();
				String sharedRootId = pathInfo.getSharedRootId();

				// do nothing if under MY_COMMUNICATION_DATA
				String[] splittedPath = path.split("/");
				if (splittedPath.length >= 1 && splittedPath[0].toLowerCase().equals(CommonFolders.MY_COMMUNICATION_DATA.toString().toLowerCase())) {
					return;
				}
				String sharedUpperPath = metadataFactory.getSharedUpperPath(pathInfo.getSharedRootId());
				String ownerSharedPath = StringUtils.trimFilePathRoot(sharedUpperPath);
				String memberSharedPath =metadataFactory.getMetadataRelativePath(pathInfo.getSharedRootId(), pathInfo.getUserId());
				String sharedPath;
				if (pathInfo.getUserId().equals(pathInfo.getOwnerId())) {
					sharedPath = ownerSharedPath;
				} else {
					sharedPath = memberSharedPath;
				}
				String oldPath = "", oldSourcePath ="", oldOwnerId = "", oldSharedRootId = "";
				if (newPathInfo != null) {
					oldPath = path;
					path = StringUtils.trimFilePathRoot(newPathInfo.getFullOwnerPath());
					oldSourcePath = sourcePath;
					sourcePath = StringUtils.trimFilePathRoot(newPathInfo.getFullSourcePath());
					oldOwnerId = ownerId;
					ownerId = newPathInfo.getOwnerId();
					oldSharedRootId = sharedRootId;
					sharedRootId = newPathInfo.getSharedRootId();
				}
				// message
				XmppSendInfo sendInfo = new XmppSendInfo();
				sendInfo.setSystemEvent(systemEvent);
				sendInfo.setLastEditUser(pathInfo.getUserId());
				sendInfo.setLastEditTime(String.valueOf(DateUtils.nowUTCTimestamp()));
				sendInfo.setPath(path);
				sendInfo.setSourcePath(sourcePath);
				sendInfo.setOwnerId(ownerId);
				sendInfo.setSharedRootId(sharedRootId);
				sendInfo.setOldPath(oldPath);
				sendInfo.setOldSourcePath(oldSourcePath);
				sendInfo.setOldOwnerId(oldOwnerId);
				sendInfo.setOldSharedRootId(oldSharedRootId);
				sendInfo.setSharedFolderPath(sharedPath);
				sendInfo.setSyncRootId(pathInfo.getSyncRootId());
				sendMessage(pathInfo.getUserId(), "all", StringUtils.writeJSON(sendInfo));

				if (sendToAllMember && pathInfo.isUnderSharedFolders()) {
					sendToAllOwnerAndMember(pathInfo, sendInfo, sendToAcceptedMemberOnly, ownerSharedPath, memberSharedPath);
				}
			}
		};
		threadPool.execute(thread);
	}

	private void sendToAllOwnerAndMember(PathInfo pathInfo, XmppSendInfo sendInfo, boolean sendToAcceptedMemberOnly, String ownerSharedPath, String memberSharedPath) {
		Map<String, String> cellphoneUserIdMap = new HashMap<String, String>();
		List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(pathInfo.getSharedRootId());
		for (CollaborateMember member : members) {
			if ((member.getAccepted() != null && member.getAccepted() == false) || (sendToAcceptedMemberOnly && member.getAccepted() == null)) {
				continue;
			}
			cellphoneUserIdMap.putAll(ssoService.findCellphoneUserIdMap(member.getCellphones()));
		}
		Set<String> userIds = new HashSet<String>();
		userIds.addAll(cellphoneUserIdMap.values());
		userIds.add(pathInfo.getOwnerId());
		userIds.remove(pathInfo.getUserId());
		for (String userId : userIds) {
			if (userId.equals(pathInfo.getOwnerId())) {
				sendInfo.setSharedFolderPath(ownerSharedPath);
			} else {
				memberSharedPath =metadataFactory.getMetadataRelativePath(pathInfo.getSharedRootId(), userId);
				sendInfo.setSharedFolderPath(memberSharedPath);
			}
			sendMessage(userId, "all", StringUtils.writeJSON(sendInfo));
		}
	}

	private XMPPTCPConnection connectAndLogin(String username, String password, String resourceId) {
		XMPPTCPConnection connection = null;
		try {
			connection = new XMPPTCPConnection(config);
			connection.connect();
			connection.login(username, password, resourceId);
			return connection;
		} catch (Exception e) {
			disconnect(connection);
		}
		return null;
	}

	private void disconnect(XMPPTCPConnection connection) {
		if (connection != null) {
			try {
				connection.disconnect();
			} catch (Exception e) {
			}
		}
	}

	private String generateResourceId(String currentDeviceId, String appName) {
		if (currentDeviceId == null) {
			currentDeviceId = "";
		}
		if (appName == null) {
			appName = "";
		}
		return String.format("%s^_^%s", currentDeviceId, appName);
	}

	private String generateJid(String regId) {
		return String.format("%s@%s", org.jivesoftware.smack.util.StringUtils.escapeNode(regId), xmppHost);
	}

}
