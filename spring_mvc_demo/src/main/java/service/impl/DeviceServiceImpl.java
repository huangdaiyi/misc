package service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import model.Device;
import model.Metadata;
import model.MobileFolders;
import model.RequestBase;
import model.SyncRelation;
import model.request.GetDeviceRequest;
import model.request.UpdateDeviceRequest;
import model.response.GetAllDeviceResponse;
import model.response.GetDeviceResponse;
import model.response.SSODevicesInfo;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import service.DeviceService;
import service.DynamoDBService;
import service.FileService;
import service.SSOService;
import service.SyncRelationService;
import utils.DateUtils;
import utils.StringUtils;
import constants.ActivityHistoryLevel;
import constants.ChangeReason;
import constants.CommonFolders;
import constants.HttpStatus;
import dao.DeviceDao;
import dao.FolderDao;
import dao.MetadataDao;
import dao.SyncRelationDao;
import exception.MetadataException;

@Service
public class DeviceServiceImpl implements DeviceService {

	@Autowired
	private DeviceDao deviceDao;
	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private FolderDao folderDao;
	@Autowired
	private SyncRelationDao syncRelationDao;
	
	@Autowired
	private SSOService ssoService;
	@Autowired
	private FileService fileService;
	@Autowired
	private SyncRelationService syncRelationService;
	@Autowired
	private DynamoDBService dynamoDBService;

	@Override
	public GetDeviceResponse getDevice(GetDeviceRequest getDeviceRequest)
			throws MetadataException {
		Device device = deviceDao.getDevice(getDeviceRequest.getUserId(), getDeviceRequest.getDeviceUid());
		SSODevicesInfo deviceInfo = ssoService
				.getDevicesByToken(getDeviceRequest);
		if (deviceInfo == null) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		} else if (device == null) {
			throw new MetadataException(HttpStatus.DEVICE_NOT_EXIST);
		}
		for (Map<String, String> ssoDevice : deviceInfo.getDevices()) {
			if (device.getDeviceId().equals(ssoDevice.get("device_id"))) {
				device.setName(ssoDevice.get("device_name"));
			}
		}
		return device.toGetDeviceResponse();
	}

	@Override
	public Boolean updateDevice(UpdateDeviceRequest updateDeviceRequest)
			throws MetadataException {
		Device device = deviceDao.getDevice(updateDeviceRequest.getUserId(), updateDeviceRequest.getDeviceUid());
		if (device == null) {
			throw new MetadataException(HttpStatus.DEVICE_NOT_EXIST);
		}
		deviceDao.update(updateDeviceRequest.getUserId(), updateDeviceRequest.getDeviceUid(), updateDeviceRequest.getSettings());
		return true;
	}

	@Override
	public GetAllDeviceResponse getAllDevice(
			RequestBase getAllDeviceRequest) throws MetadataException {
		GetAllDeviceResponse getAllDeviceResponses = new GetAllDeviceResponse();
		List<GetDeviceResponse> deviceResponses = new ArrayList<GetDeviceResponse>();
		getAllDeviceResponses.setDevices(deviceResponses);
		List<Device> devices = deviceDao.getDevices(getAllDeviceRequest
				.getUserId());
		SSODevicesInfo deviceInfo = ssoService
				.getDevicesByToken(getAllDeviceRequest);
		if (deviceInfo == null) {
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
		for (Device device : devices) {
			for (Map<String, String> ssoDevice : deviceInfo.getDevices()) {
				if (device.getDeviceId().equals(ssoDevice.get("device_id"))) {
					device.setName(ssoDevice.get("device_name"));
					device.setStatus(ssoDevice.get("device_status"));
				}

			}
			deviceResponses.add(device.toGetDeviceResponse());
		}
		return getAllDeviceResponses;
	}

	@Override
	public GetDeviceResponse createDevice(RequestBase requestBase)
			throws MetadataException {
		String uniqueId = buildUniqueId(requestBase.getUserId(),
				requestBase.getDeviceId());
		Device device = new Device();
		device.setDeviceId(requestBase.getDeviceId());
		device.setDeviceUid(uniqueId);
		device.setLastEditUser(requestBase.getUserId());
		device.setLastEditTime(DateUtils.nowUTCTimestamp());
		device.setUserId(requestBase.getUserId());
		deviceDao.createDevice(device);
		return device.toGetDeviceResponse();
	}

	private String buildUniqueId(String userId, String deviceId) {
		Random ran = new Random();
		StringBuilder uniqueId = new StringBuilder();
		uniqueId.append(userId);
		uniqueId.append(":");
		uniqueId.append(deviceId);
		uniqueId.append(":");
		uniqueId.append(ran.nextInt(100000));
		uniqueId.append(":");
		uniqueId.append(DateUtils.nowUTCTimestamp());
		uniqueId.append(":");
		uniqueId.append(StringUtils.getUUID());
		return StringUtils.getMD5(uniqueId.toString());

	}
	
	@Override
	@Transactional
	public void unregister(String userId, String deviceId) throws MetadataException {
		// get device
		Device device = deviceDao.getDeviceByDeviceId(userId, deviceId);
		if(device == null) {
			throw new MetadataException(HttpStatus.DEVICE_NOT_EXIST);
		}
		
		// delete device
		deviceDao.deleteDevice(device.getId());
		
		// delete metadata (file)
		List<Metadata> files = metadataDao.getAllFilesByDeviceUid(device.getDeviceUid());
		for(Metadata file : files) {
			fileService.deleteFile(file);
		}
		
		// delete relations and empty folders
		List<SyncRelation> relations = syncRelationDao.getLowerLevelFoldersOfClient(userId,
				CommonFolders.MY_DEVICE_SYNC_FOLDERS.toString() + "/" + device.getDeviceUid(), true);
		syncRelationService.deleteSyncRelations(userId, relations);
		for(SyncRelation relation : relations) {
			syncRelationService.cleanSynRelationRelatedEmptyFolders(userId, relation);
		}
		
		// delete link folders
		List<MobileFolders> mobileFolders = folderDao.getQueryFolders(userId, null, "");
		List<Integer> idList = new ArrayList<Integer>();
		for(MobileFolders mobileFolder : mobileFolders) {
			idList.add(mobileFolder.getId());
		}
		if(idList.size() > 0) {
			folderDao.deleteMobileFolderByIds(idList);
		}
		
		// activity history
		addActivityHistory(userId, ChangeReason.DEVICE_UNREGISTER);
	}
	
	private void addActivityHistory(String userId, ChangeReason reason) {
		dynamoDBService.createActivityHistory("", "", "", "", "",
				userId, DateUtils.nowUTCDateTime(), reason.toString(), "",
				ActivityHistoryLevel.NORMAL.toString());
	}
}
