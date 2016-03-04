package factory.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import model.BackupMetadata;
import model.BaseMetadata;
import model.CellPhone;
import model.CollaborateMember;
import model.Device;
import model.InviteProcessingStatus;
import model.ItemStyle;
import model.Mail;
import model.Metadata;
import model.MetadataAttr;
import model.PageProfileProperty;
import model.PathInfo;
import model.PathRequestBase;
import model.ProfileProperty;
import model.RequestBase;
import model.response.FileResponse;
import model.response.FolderResponse;
import model.response.GetCollaborateInfoResponse;
import model.response.SSODevicesInfo;

import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import service.CollaborateService;
import service.DynamoDBService;
import service.FileService;
import service.FolderService;
import service.SSOService;
import utils.CellphonesUtils;
import utils.DateUtils;
import utils.MetadataUtils;
import utils.StringUtils;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import constants.CommonFolders;
import constants.HttpStatus;
import constants.MetadataType;
import constants.SortType;
import dao.BackupMetadataDao;
import dao.CollaborateDao;
import dao.CountDao;
import dao.DeviceDao;
import dao.MetadataAttrDao;
import dao.MetadataDao;
import dao.ProfilePropertyDao;
import exception.MetadataException;
import factory.MetadataFactory;
import factory.MetadataResponseComparatorFactory;
import factory.PathFactory;

@Component
public class MetadataFactoryImpl implements MetadataFactory {

	@Value("${gateway.external.url}")
	private String gatewayExternalUrl;

	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private ProfilePropertyDao profilePropertyDao;
	@Autowired
	private MetadataResponseComparatorFactory metadataResponseComparatorFactory;
	@Autowired
	private BackupMetadataDao backupMetadataDao;
	@Autowired
	private DeviceDao deviceDao;
	@Autowired
	private SSOService ssoService;
	@Autowired
	private FolderService folderService;
	@Autowired
	private FileService fileService;
	@Autowired
	private CollaborateService collaborateService;
	@Autowired
	private CountDao countDao;
	@Autowired
	private PathFactory pathFactory;
	@Autowired
	private CollaborateDao collaborateDao;
	@Autowired
	private DynamoDBService dynamoDBService;
	@Autowired
	private MetadataAttrDao metadataAttrDao;

	@Override
	public String generateNameForMoveCopy(BaseMetadata metadata,
			boolean isFolder, boolean isMove) {
		List<Metadata> list = metadataDao.getMetadatasByParentId(metadata
				.getParentId());
		Set<String> set = new HashSet<String>();
		for (Metadata m : list) {
			set.add(m.getOriginName());
		}
		String originalName = metadata.getOriginName();
		String baseName;
		String ext;

		int index = originalName.indexOf(".");
		if (!isFolder && index > -1) {
			baseName = originalName.substring(0, index);
			ext = originalName.substring(index);
		} else {
			baseName = originalName;
			ext = "";
		}
		String newName;
		for (int i = 1;; i++) {
			if (isMove) {
				newName = String.format("%s (%s)%s", baseName, i, ext);
			} else {
				newName = String.format("%s - Copy(%s)%s", baseName, i, ext);
			}
			if (!set.contains(newName)) {
				return newName;
			}
		}
	}

	@Override
	public FolderResponse toFolderResponse(BaseMetadata folder,
			boolean displayNote, Map<String, Object> appendOtherParams) {
		String parentPath = FilenameUtils
				.getFullPathNoEndSeparator(getMetadataOriginalPath(folder));
		return toFolderResponse(folder, displayNote, parentPath,
				appendOtherParams);
	}

	@Override
	public FolderResponse toFolderResponse(BaseMetadata folder,
			boolean displayNote, String parentPath) {
		return toFolderResponse(folder, displayNote, parentPath, null);
	}

	@Override
	public FolderResponse toFolderResponse(BaseMetadata folder,
			boolean displayNote, String parentPath,
			Map<String, Object> appendOtherParams) {
		FolderResponse folderResponse = FolderResponse.parse(folder,
				displayNote, parentPath, appendOtherParams);
		// for my shared folders as member to get count and size
		if (folder.getType().equals(MetadataType.SHARECOMMON.toString())) {
			List<CellPhone> inquirerCellphones = dynamoDBService
					.findCellphonesByUserId(folder.getOwnerId());
			String cellphones = CellphonesUtils
					.composeCellphones(inquirerCellphones);
			inquirerCellphones = null;
			List<CollaborateMember> selfBeMembers = collaborateDao
					.getCollaborateMembersByCellphones(cellphones, false, false, false);
			int folderCount = 0;
			int fileCount = 0;
			long totalSize = 0;
			int subFolderCount = 0;
			for (CollaborateMember collaborateMember : selfBeMembers) {
				if (collaborateMember.getAccepted() != null && collaborateMember.getAccepted() == false) {
					continue;
				}
				subFolderCount += 1;
				if (collaborateMember.getAccepted() != null){
					MetadataAttr folderAttr = countDao.queryCount(collaborateMember
							.getMetadataIndexId());
					folderCount = folderCount + folderAttr.getFoldersCount();
					fileCount = fileCount + folderAttr.getFilesCount();
					totalSize = totalSize + folderAttr.getTotalSize();
				}
			}
			folderResponse.setFilesCount(fileCount);
			folderResponse.setFoldersCount(folderCount+subFolderCount);
			folderResponse.setSubFolderCount(subFolderCount);
			folderResponse.setSubFileCount(0);
			folderResponse.setTotalSize(totalSize);
		}else if (folder.getType().equals(MetadataType.REFLINK.toString())) {
			Metadata sharedFolder = metadataDao.getMetadata(folder.getRefId());
			List<Metadata> sharedSub = metadataDao.getMetadatasByParentId(sharedFolder.getId());
			int subFileCount = 0;
			for (Metadata metadata : sharedSub) {
				if (!metadata.isFolder()) {
					subFileCount++;
				}
			}
			folderResponse.setType(MetadataType.SHARE);
			folderResponse.setSubFileCount(subFileCount);
			folderResponse.setSubFolderCount(sharedSub.size()-subFileCount);
			folderResponse.setTotalSize(sharedFolder.getTotalSize());
			folderResponse.setHasNote(sharedFolder.hasNote());
			folderResponse.setSharedRootId(sharedFolder.getSharedRootId());
			folderResponse.setOwnerId(sharedFolder.getOwnerId());
			folderResponse.setIconBlockId(sharedFolder.getIconBlockId());
			folderResponse.setIconText(sharedFolder.getIconText());
			folderResponse.setIconTextColor(sharedFolder.getIconTextColor());
			folderResponse.setIconTextStyle(sharedFolder.getIconTextStyle());
			if (displayNote) {
				folderResponse.getOtherParams().put("note_content", sharedFolder.getNote());
			}
			
			
		} else {
			int subFolderCount = 0;
			int subFileCount = 0;
			if (folder.isBackup()) {
				subFolderCount = (int) backupMetadataDao.countSubBackupsByParentId(folder.getId(), true);
				subFileCount = (int) backupMetadataDao.countSubBackupsByParentId(folder.getId(), false);
			} else {
				subFolderCount = (int) metadataDao.countSubMetadatasByParentId(folder.getId(), true);
				subFileCount = (int) metadataDao.countSubMetadatasByParentId(folder.getId(), false);
			}
			MetadataAttr folderAttr = countDao.queryCount(folder.getId());
			folderResponse.setFilesCount(folderAttr.getFilesCount());
			folderResponse.setFoldersCount(folderAttr.getFoldersCount());
			folderResponse.setSubFolderCount(subFolderCount);
			folderResponse.setSubFileCount(subFileCount);
		}
		return folderResponse;
	}

	@Override
	public List<FolderResponse> toFolderResponses(
			List<? extends BaseMetadata> folders, boolean displayNote,
			String parentPath) {
		List<FolderResponse> result = new ArrayList<FolderResponse>();
		if (folders == null || folders.isEmpty()) {
			return result;
		}
		Map<String, String> displayNameMap = new HashMap<String, String>();
		for (BaseMetadata metadata : folders) {
			if (metadata.isFolder() == false) {
				continue;
			}
			// bulk correct attrs
			String displayName = displayNameMap.get(metadata.getCreatedBy());
			if (displayName == null) {
				displayName = ssoService.findDisplayNameByUserId(metadata
						.getCreatedBy());
				displayNameMap.put(metadata.getCreatedBy(), displayName);
			}
			FolderResponse folderResponse = toFolderResponse(metadata,
					displayNote, parentPath);
			folderResponse.setCreatedBy(displayName);
			result.add(folderResponse);
		}
		return result;
	}

	@Override
	public List<FolderResponse> updateFolderResponsesAfterPaging(List<FolderResponse> folderResponses, String userId, String ownerId) {
		if (folderResponses == null || folderResponses.isEmpty()) {
			return folderResponses;
		}
		for (FolderResponse folderResponse : folderResponses) {
			// udpate has_unread_file
			boolean hasUnreadFile = hasUnreadFile(folderResponse.getId(), userId);
			folderResponse.setHasUnreadFile(hasUnreadFile);
			
			// add has_inviting for only MY_SHARED_FOLDERS
			if(folderResponse.getPath().toLowerCase().equals(CommonFolders.MY_SHARED_FOLDERS.toString().toLowerCase())) {
				boolean hasInviting = false;
				
				List<CellPhone> inquirerCellphones = dynamoDBService.findCellphonesByUserId(userId);
				String cellphones = CellphonesUtils.composeCellphones(inquirerCellphones);
				List<CollaborateMember> collaborateMembers = collaborateDao.getCollaborateMembersByCellphones(cellphones, false, false, false);
				
				for(CollaborateMember collaborateMember : collaborateMembers) {
					if(collaborateMember.getAccepted() == null) {
						hasInviting = true;
						break;
					}
				}
				
				Map<String, Object> otherParams = folderResponse.getOtherParams();
				otherParams.put("has_inviting", hasInviting);
			}
			
			// update members count
			String metadataIndexId = "";
			if (folderResponse.getType().equals(MetadataType.SHARE)) {
				metadataIndexId = folderResponse.getSharedRootId();
			}else {
				metadataIndexId = folderResponse.getId();
			}
			folderResponse.setMembersCount(0);
			List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(metadataIndexId);
			for (CollaborateMember member : members) {
				if (member.getAccepted() == null || member.getAccepted() == true) {
					folderResponse.setMembersCount(folderResponse.getMembersCount() + 1);
				}
			}
		}
		adjustFolderResponsesItemStyleByGlobalItemStyle(folderResponses, ownerId);
		return folderResponses;
	}

	private void adjustFolderResponsesItemStyleByGlobalItemStyle(List<FolderResponse> metadataList, String ownerId) {
		ItemStyle globalItemStyle = metadataDao.getGlobalItemStyle(ownerId);
		for (FolderResponse metadata : metadataList) {
			if (metadata.getItemTextColor() == null) {
				metadata.setItemTextColor(globalItemStyle.getItemTextColor());
			}
			if (metadata.getItemBgColor() == null) {
				metadata.setItemBgColor(globalItemStyle.getItemBgColor());
			}
			if (metadata.isItemTextBold() == null) {
				metadata.setItemTextBold(globalItemStyle.isItemTextBold());
			}
			if (metadata.isItemTextItalic() == null) {
				metadata.setItemTextItalic(globalItemStyle.isItemTextItalic());
			}
			if (metadata.isItemTextUnderline() == null) {
				metadata.setItemTextUnderline(globalItemStyle.isItemTextUnderline());
			}
		}
	}

	@Override
	public FileResponse toFileResponse(BaseMetadata file, boolean displayNote,
			Map<String, Object> appendOtherParams) {
		String parentPath = FilenameUtils
				.getFullPathNoEndSeparator(getMetadataOriginalPath(file));
		return toFileResponse(file, displayNote, parentPath, appendOtherParams);
	}

	@Override
	public FileResponse toFileResponse(BaseMetadata file, boolean displayNote,
			String parentPath) {
		return toFileResponse(file, displayNote, parentPath, null);
	}

	@Override
	public FileResponse toFileResponse(BaseMetadata file, boolean displayNote,
			String parentPath, Map<String, Object> appendOtherParams) {
		return FileResponse.parse(file, displayNote, gatewayExternalUrl, parentPath,
				appendOtherParams);
	}

	@Override
	public List<FileResponse> toFileResponses(String token,
			List<? extends BaseMetadata> files, boolean displayNote,
			String parentPath) {
		List<FileResponse> result = new ArrayList<FileResponse>();
		if (files == null || files.isEmpty()) {
			return result;
		}
		Map<String, Device> deviceMap = new HashMap<String, Device>();
		Map<String, String> ssoDeviceMap = null;
		Map<String, String> displayNameMap = new HashMap<String, String>();
		for (BaseMetadata metadata : files) {
			if (metadata.isFolder()) {
				continue;
			}
			// bulk correct attrs
			Device device = null;
			if (StringUtils.isNullOrEmpty(metadata.getDeviceUid()) == false) {
				String deviceMapKey = String.format("%s_%s",
						metadata.getCreatedBy(), metadata.getDeviceUid());
				device = deviceMap.get(deviceMapKey);
				if (device == null) {
					device = deviceDao.getDevice(metadata.getCreatedBy(),
							metadata.getDeviceUid());
					if (device != null) {
						if (ssoDeviceMap == null) {
							ssoDeviceMap = buildSsoDeviceMap(token);
						}
						String deviceName = ssoDeviceMap.get(device.getDeviceId());
						device.setName(deviceName != null ? deviceName : "");
						deviceMap.put(deviceMapKey, device);
					}
				}
			}
			String displayName = displayNameMap.get(metadata.getCreatedBy());
			if (displayName == null) {
				displayName = ssoService.findDisplayNameByUserId(metadata
						.getCreatedBy());
				displayNameMap.put(metadata.getCreatedBy(), displayName);
			}
			FileResponse fileResponse = toFileResponse(metadata, displayNote,
					parentPath);
			fileResponse.setCreatedBy(displayName);
			fileResponse.setDeviceName(device != null ? device.getName() : "");
			result.add(fileResponse);
		}
		return result;
	}

	@Override
	public List<FileResponse> updateFileResponsesAfterPaging(List<FileResponse> fileResponses, String userId, BaseMetadata metadata, String ownerId) {
		if (fileResponses == null || fileResponses.isEmpty()) {
			return fileResponses;
		}
		fileResponses = fileService.updateAlreadyReadAndCount(userId, metadata, fileResponses, true);
		adjustFileResponsesItemStyleByGlobalItemStyle(fileResponses, ownerId);
		return fileResponses;
	}

	@Override
	public void adjustFileResponsesItemStyleByGlobalItemStyle(List<FileResponse> metadataList, String ownerId) {
		ItemStyle globalItemStyle = metadataDao.getGlobalItemStyle(ownerId);
		for (FileResponse metadata : metadataList) {
			if (metadata.getItemTextColor() == null) {
				metadata.setItemTextColor(globalItemStyle.getItemTextColor());
			}
			if (metadata.getItemBgColor() == null) {
				metadata.setItemBgColor(globalItemStyle.getItemBgColor());
			}
			if (metadata.isItemTextBold() == null) {
				metadata.setItemTextBold(globalItemStyle.isItemTextBold());
			}
			if (metadata.isItemTextItalic() == null) {
				metadata.setItemTextItalic(globalItemStyle.isItemTextItalic());
			}
			if (metadata.isItemTextUnderline() == null) {
				metadata.setItemTextUnderline(globalItemStyle.isItemTextUnderline());
			}
		}
	}

	private Map<String, String> buildSsoDeviceMap(String token) {
		Map<String, String> deviceMap = new HashMap<String, String>();
		SSODevicesInfo deviceInfo = ssoService
				.getDevicesByToken(new RequestBase(token));
		if (deviceInfo != null) {
			for (Map<String, String> ssoDevice : deviceInfo.getDevices()) {
				deviceMap.put(ssoDevice.get("device_id"),
						ssoDevice.get("device_name"));
			}
		}
		return deviceMap;
	}

	@Override
	public void sortFolderResponses(List<FolderResponse> folderResponses,
			SortType sortType) {
		Collections.sort(folderResponses, metadataResponseComparatorFactory
				.generateFolderResponseComparator(sortType));
	}

	@Override
	public void sortFileResponses(List<FileResponse> fileResponses,
			SortType sortType) {
		Collections.sort(fileResponses, metadataResponseComparatorFactory
				.generateFileResponseComparator(sortType));
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> List<T> pagingFolderResponses(List<T> folderResponses, int pageNumber, int itemsPerPage, int prefixCount) {
		if (pageNumber <= 0 || itemsPerPage <= 0) {
			return folderResponses;
		}
		int fromIndex = (pageNumber - 1) * itemsPerPage;
		int toIndex = fromIndex + itemsPerPage;
		// simulate size
		List<T> newFolderResponses = new ArrayList<T>();
		if (prefixCount > 0) {
			newFolderResponses.addAll(Arrays.asList((T[]) new Object[prefixCount]));
		}
		newFolderResponses.addAll(folderResponses);
		// sublist
		try {
			return newFolderResponses.subList(Math.max(fromIndex, prefixCount), Math.min(toIndex, newFolderResponses.size()));
		} catch (Exception e) {
		}
		return new ArrayList<T>();
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> List<T> pagingFileResponses(List<T> fileResponses, int pageNumber, int itemsPerPage, int prefixCount) {
		if (pageNumber <= 0 || itemsPerPage <= 0) {
			return fileResponses;
		}
		int fromIndex = (pageNumber - 1) * itemsPerPage;
		int toIndex = fromIndex + itemsPerPage;
		// simulate size
		List<T> newFileResponses = new ArrayList<T>();
		if (prefixCount > 0) {
			newFileResponses.addAll(Arrays.asList((T[]) new Object[prefixCount]));
		}
		newFileResponses.addAll(fileResponses);
		// sublist
		try {
			return newFileResponses.subList(Math.max(fromIndex, prefixCount), Math.min(toIndex, newFileResponses.size()));
		} catch (Exception e) {
		}
		return new ArrayList<T>();
	}

	@Override
	public PageProfileProperty computePageProfileProperty(PathInfo pathInfo, String metadataIndexId, String viewerDeviceUniqueId, SortType forceSortByType) {
		PageProfileProperty pageProfileProperty = new PageProfileProperty();
		// get sortby_value and viewby_value
		List<ProfileProperty> properties = profilePropertyDao.getProfileProperties(pathInfo.getOwnerId(), metadataIndexId);
		List<ProfileProperty> refProperties = null;
		if (pathInfo.isUnderMyBackupData()) {
			try {
				Metadata formalMetadata = metadataDao.getMetadataByPath(MetadataUtils.fullBackupPathToNormalPath(pathInfo.getFullOwnerPath()), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
				if (formalMetadata != null) {
					refProperties = profilePropertyDao.getProfileProperties(pathInfo.getOwnerId(), formalMetadata.getId());
				}
			} catch (MetadataException e) {
				// ignore not found excetpion
			}
			pageProfileProperty.resetSortByType(refProperties, forceSortByType);
		} else {
			pageProfileProperty.resetSortByType(properties, forceSortByType);
		}
		pageProfileProperty.resetViewByType(properties, viewerDeviceUniqueId);
		pageProfileProperty.resetGridByType(properties, viewerDeviceUniqueId);
		return pageProfileProperty;
	}

	private String getBackupMetadataOriginalPath(BaseMetadata metadata) {
		if (metadata == null || metadata.getParentId().isEmpty()) {
			return CommonFolders.MY_BACKUP_DATA.getOriginPath();
		}
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(metadata.getOriginName());
		metadata = backupMetadataDao.getBackup(metadata.getParentId());
		while (metadata != null && metadata.getParentId().isEmpty() == false) {
			stringBuilder.insert(0, metadata.getOriginName().concat("/"));
			metadata = backupMetadataDao.getBackup(metadata.getParentId());
		}
		stringBuilder.insert(0, CommonFolders.MY_BACKUP_DATA.getOriginPath()
				.concat("/"));
		return stringBuilder.toString();
	}

	// not include ownerId
	@Override
	public String getMetadataOriginalPath(BaseMetadata metadata) {
		if (metadata == null) {
			return "";
		} else if (metadata instanceof BackupMetadata || metadata.isBackup()) {
			return getBackupMetadataOriginalPath(metadata);
		} else if (metadata.getParentId().isEmpty()) {
			return "";
		}
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(metadata.getOriginName());
		metadata = metadataDao.getMetadata(metadata.getParentId());
		while (metadata != null && metadata.getParentId().isEmpty() == false) {
			stringBuilder.insert(0, metadata.getOriginName().concat("/"));
			metadata = metadataDao.getMetadata(metadata.getParentId());
		}
		return stringBuilder.toString();
	}

	private String getMetadataRelativePathForMember(
			BaseMetadata metadata,String userId) {
		Metadata refMetadata = metadataDao.getMetadataByRefId(userId, metadata.getSharedRootId());
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(metadata.getOriginName());
		metadata = metadataDao.getMetadata(metadata.getParentId());
		while (metadata != null && metadata.getParentId().isEmpty() == false
				&& metadata.getSharedRootId().isEmpty() == false) {
			stringBuilder.insert(0, metadata.getOriginName().concat("/"));
			if (metadata.getId().equals(metadata.getSharedRootId()) == true) {
				break;
			}
			metadata = metadataDao.getMetadata(metadata.getParentId());
		}
		if (refMetadata == null) {
			stringBuilder.insert(0, CommonFolders.MY_SHARED_FOLDERS.getOriginPath()
					.concat("/"));
		}else{
			String fullPath = metadataDao.getFullPath(refMetadata.getId());
			fullPath = StringUtils.trimFilePathRoot(FilenameUtils.getPathNoEndSeparator(fullPath));
			fullPath = StringUtils.isNullOrEmpty(fullPath)? "":fullPath.concat("/");
			stringBuilder.insert(0, fullPath);
		}
		
		return stringBuilder.toString();
	}

	// not include ownerId
	// owner = original path
	// member = my shared folders/xxx/ooo.jpg
	@Override
	public String getMetadataRelativePath(String metadataId, String viewerId){
		Metadata metadata = metadataDao.getMetadata(metadataId);
		return getMetadataRelativePath(metadata, viewerId);
	}
	@Override
	public String getMetadataRelativePath(BaseMetadata metadata, String viewerId) {
		if (metadata == null
				|| (metadata.getParentId().isEmpty() && metadata instanceof Metadata)) {
			return "";
		} else if (metadata instanceof BackupMetadata || metadata.isBackup()) {
			return getBackupMetadataOriginalPath(metadata);
		} else if (metadata.getSharedRootId().isEmpty() == false
				&& metadata.getOwnerId().equals(viewerId) == false) {
			return getMetadataRelativePathForMember(metadata,viewerId);
		}
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(metadata.getOriginName());
		metadata = metadataDao.getMetadata(metadata.getParentId());
		while (metadata != null && metadata.getParentId().isEmpty() == false) {
			stringBuilder.insert(0, metadata.getOriginName().concat("/"));
			metadata = metadataDao.getMetadata(metadata.getParentId());
		}
		return stringBuilder.toString();
	}

	@Override
	public String getSharedUpperPath(String sharedRootId) {
		Metadata sharedFolder = metadataDao.getMetadata(sharedRootId);

		return getSharedUpperPath(sharedFolder);
	}

	@Override
	public String getSharedUpperPath(Metadata sharedFolder) {
		String sharedUpperPath = "";
		if (sharedFolder != null) {
			sharedUpperPath = sharedFolder.getOwnerId() + "/"
					+ getMetadataOriginalPath(sharedFolder);
		}
		return sharedUpperPath;
	}

	@Override
	public List<Metadata> getAllSubFoldersAndFiles(Metadata metadata) {
		List<Metadata> resultList = new ArrayList<Metadata>();
		List<Metadata> checkList = new ArrayList<Metadata>();
		List<Metadata> tempList;
		Metadata parentMetadata;
		String parentPath;
		metadata.setPath(getMetadataOriginalPath(metadata));
		checkList.add(metadata);
		while (checkList.size() > 0) {
			parentMetadata = checkList.get(0);
			tempList = metadataDao.getMetadatasByParentId(parentMetadata
					.getId());
			resultList.addAll(tempList);
			parentPath = parentMetadata.getPath();
			for (int i = 0; i < tempList.size(); i++) {
				metadata = tempList.get(i);
				metadata.setPath(String.format("%s/%s", parentPath,
						metadata.getOriginName()));
				if (metadata.isFolder()) {
					checkList.add(metadata);
				}
			}
			checkList.remove(0);
		}
		return resultList;
	}

	@Override
	public List<Metadata> getAllSubFolders(Metadata metadata) {
		List<Metadata> resultList = new ArrayList<Metadata>();
		List<Metadata> checkList = new ArrayList<Metadata>();
		List<Metadata> tempList;
		Metadata parentMetadata;
		checkList.add(metadata);
		while (checkList.size() > 0) {
			parentMetadata = checkList.get(0);
			tempList = metadataDao.getFoldersByParentId(parentMetadata.getId());
			resultList.addAll(tempList);
			for (int i = 0; i < tempList.size(); i++) {
				checkList.add(tempList.get(i));
			}
			checkList.remove(0);
		}
		return resultList;
	}

	@Override
	public List<Metadata> getAllUpperLevelMetadatas(Metadata metadata) {
		List<Metadata> list = new ArrayList<Metadata>();
		while (!metadata.getParentId().equals("")) {
			metadata = metadataDao.getMetadata(metadata.getParentId());
			list.add(metadata);
		}
		return list;
	}

	@Override
	public List<MetadataAttr> generateMetadataAttrsForUpdateParents(
			Metadata metadata, BackupMetadata backupMetadata, int foldersCount,
			int filesCount, long totalSize) {
		List<MetadataAttr> list = new ArrayList<MetadataAttr>();
		if (metadata != null) {
			while (!metadata.getParentId().equals("")) {
				metadata = metadataDao.getMetadata(metadata.getParentId());
				list.add(new MetadataAttr(metadata.getId(), false,
						foldersCount, filesCount, totalSize, 0, 0));
			}
		}
		if (backupMetadata != null) {
			while (!backupMetadata.getParentId().equals("")) {
				backupMetadata = backupMetadataDao.getBackup(backupMetadata
						.getParentId());
				list.add(new MetadataAttr(backupMetadata.getId(), true,
						foldersCount, filesCount, totalSize, 0, 0));
			}
		}
		return list;
	}

	@Override
	public BaseMetadata getBaseMetadata(String userId, String path,
			String sourcePath, String ownerId, String sharedRootId,
			boolean isFolder) {
		PathRequestBase pathRequestBase = new PathRequestBase();
		pathRequestBase.setUserId(userId);
		pathRequestBase.setPath(path);
		pathRequestBase.setSourcePath(sourcePath);
		pathRequestBase.setOwnerId(ownerId);
		pathRequestBase.setSharedRootId(sharedRootId);
		PathInfo pathInfo = pathFactory
				.parsePathInfo(pathRequestBase, isFolder, true);
		return getBaseMetadataByPathInfo(pathInfo);
	}

	@Override
	public BaseMetadata getFile(String userId, String path, String sourcePath,
			String ownerId, String sharedRootId) {
		BaseMetadata file = getBaseMetadata(userId, path, sourcePath, ownerId,
				sharedRootId, false);
		if (file == null || file.isFolder() == true) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		return file;
	}

	@Override
	public BaseMetadata getFolder(String userId, String path, String ownerId,
			String sharedRootId) {
		BaseMetadata folder = getBaseMetadata(userId, path, "", ownerId,
				sharedRootId, true);
		if (folder == null || folder.isFolder() == false) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}
		return folder;
	}

	@Override
	public BaseMetadata getBaseMetadataByPathInfo(PathInfo pathInfo) {
		BaseMetadata file;
		if (pathInfo.isUnderMyBackupData()) {
			file = backupMetadataDao.getBackupMetadataByPath(
					pathInfo.generateBackupFullOwnerPath(),
					pathInfo.getDeviceUid(), pathInfo.getFullSourcePath(),
					pathInfo.isFolder());
		} else {
			file = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
					pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		}
		if (file != null) {
			String id = file.getId();
			MetadataAttr metadataAttr = metadataAttrDao.getMetadataAttr(id);
			file.setFilesCount(metadataAttr.getFilesCount());
			file.setFoldersCount(metadataAttr.getFoldersCount());
			file.setTotalSize(metadataAttr.getTotalSize());
			file.setCollaborateCount(metadataAttr.getCollaborateCount());
		}
		return file;
	}

	@Override
	public BaseMetadata getFileByPathInfo(PathInfo pathInfo) {
		BaseMetadata file = getBaseMetadataByPathInfo(pathInfo);
		if (file == null || file.isFolder() == true) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		return file;
	}

	@Override
	public BaseMetadata getFolderByPathInfo(PathInfo pathInfo) {
		BaseMetadata folder = getBaseMetadataByPathInfo(pathInfo);
		if (folder == null || folder.isFolder() == false) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}
		return folder;
	}

	@Override
	public BaseMetadata getBaseMetadataById(String id, boolean isBackup) {
		if (isBackup) {
			return backupMetadataDao.getBackup(id);
		}
		return metadataDao.getMetadata(id);
	}

	@Override
	public BaseMetadata getBaseMetadataByPath(PathInfo pathInfo) {
		if (pathInfo.isUnderMyAppSyncFolders()) {
			return metadataDao.getSyncAppMetadataByPath(pathInfo
					.getFullOwnerPath(), pathInfo.getDeviceUid(),
					pathInfo.getFullSourcePath());
		} else if (pathInfo.isUnderMyBackupData()) {
			return backupMetadataDao.getBackupMetadataByPath(pathInfo
					.generateBackupFullOwnerPath(), pathInfo
					.getDeviceUid(), pathInfo.getFullSourcePath(), pathInfo
					.isFolder());
		}
		return metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(),
				pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
	}

	@Override
	public List<? extends BaseMetadata> getBaseMetadatasByParentId(
			String metadataIndexId, boolean isUnderMyBackupData,
			boolean includeInvisible) {
		List<? extends BaseMetadata> result = null;
		if (isUnderMyBackupData) {
			result = backupMetadataDao
					.getBackupMetadatasByParentId(metadataIndexId);
		} else {
			result = metadataDao.getMetadatasByParentId(metadataIndexId);
		}
		if (includeInvisible == false) {
			result = Lists.newArrayList(Iterables.filter(result,
					new Predicate<BaseMetadata>() {
						@Override
						public boolean apply(BaseMetadata metadata) {
							return metadata.isVisible();
						}
					}));
		}
		return result;
	}

	@Override
	public List<Metadata> parseBaseMetadataToMetadata(
			List<BackupMetadata> baseMetadatas) {
		List<Metadata> metadatas = new ArrayList<Metadata>();
		for (BackupMetadata baseMetadata : baseMetadatas) {
			Metadata metadata = new Metadata();
			metadata.setId(baseMetadata.getId());
			metadata.setParentId(baseMetadata.getParentId());
			metadata.setName(baseMetadata.getName());
			metadata.setFolder(baseMetadata.isFolder());
			metadata.setSortPriority(baseMetadata.getSortPriority());
			metadata.setSize(baseMetadata.getSize());
			metadata.setOriginName(baseMetadata.getOriginName());
			metadata.setModifiedAt(baseMetadata.getModifiedAt());
			metadata.setModifiedBy(baseMetadata.getModifiedBy());
			metadata.setDeviceUid(baseMetadata.getDeviceUid());
			metadata.setFullSourcePath(baseMetadata.getFullSourcePath());
			metadata.setNote(baseMetadata.getNote());
			metadata.setParams(baseMetadata.getParams());
			metadata.setIconBlockId(baseMetadata.getIconBlockId());
			metadata.setEncrypted(baseMetadata.isEncrypted());
			metadata.setCreatedAt(baseMetadata.getCreatedAt());
			metadata.setCreatedBy(baseMetadata.getCreatedBy());
			metadata.setBlockId(baseMetadata.getBlockId());
			metadata.setType(MetadataType.NORMAL.toString());
			metadata.setOwnerId(baseMetadata.getOwnerId());
			metadata.setTotalSize(baseMetadata.getTotalSize());
			metadata.setFoldersCount(baseMetadata.getFoldersCount());
			metadata.setFilesCount(baseMetadata.getFilesCount());
			metadata.setWidth(baseMetadata.getWidth());
			metadata.setHeight(baseMetadata.getHeight());
			metadatas.add(metadata);
		}
		return metadatas;
	}

	@Override
	public GetCollaborateInfoResponse toGetCollaborateInfoResponse(
			Metadata collaborate, String userId) {
		String path = getMetadataRelativePath(collaborate, userId);
		List<Metadata> subMetadatas = metadataDao
				.getMetadatasByParentId(collaborate.getId());
		int subFolderCount = 0;
		for (Metadata metadata : subMetadatas) {
			if (metadata.isFolder()) {
				subFolderCount += 1;
			}
		}
		String ownerDisplayName = ssoService
				.findDisplayNameByUserId(collaborate.getOwnerId());
		GetCollaborateInfoResponse getCollaborateInfoResponse = new GetCollaborateInfoResponse();
		getCollaborateInfoResponse.setName(collaborate.getOriginName());
		getCollaborateInfoResponse.setPath(path);
		getCollaborateInfoResponse.setOwnerId(collaborate.getOwnerId());
		getCollaborateInfoResponse.setOwner(ownerDisplayName);
		getCollaborateInfoResponse.setIconBlockId(collaborate.getIconBlockId());
		getCollaborateInfoResponse.setIconText(collaborate.getIconText());
		getCollaborateInfoResponse.setIconTextColor(collaborate
				.getIconTextColor());
		getCollaborateInfoResponse.setIconTextStyle(collaborate
				.getIconTextStyle());
		getCollaborateInfoResponse.setFilesCount(collaborate.getFilesCount());
		getCollaborateInfoResponse.setFoldersCount(collaborate
				.getFoldersCount());
		getCollaborateInfoResponse.setAccepted(null);
		getCollaborateInfoResponse.setMemberId(null);
		getCollaborateInfoResponse.setTotalSize(collaborate.getTotalSize());
		getCollaborateInfoResponse.setSubFolderCount(subFolderCount);
		getCollaborateInfoResponse.setSubFileCount(subMetadatas.size()
				- subFolderCount);
		getCollaborateInfoResponse.setMetadataIndexId(collaborate
				.getSharedRootId());
		return getCollaborateInfoResponse;
	}

	@Override
	public List<GetCollaborateInfoResponse> updateGetCollaborateInfoResponseAfterPaging(List<GetCollaborateInfoResponse> folderResponses, String userId) {
		List<CollaborateMember> allMembers = new ArrayList<CollaborateMember>();
		for (GetCollaborateInfoResponse folderResponse : folderResponses) {
			boolean hasUnreadFile = hasUnreadFile(folderResponse.getMetadataIndexId(), userId);
			folderResponse.setHasUnreadFile(hasUnreadFile);
			allMembers.addAll(folderResponse.getMembers());

			for (CollaborateMember member : folderResponse.getMembers()) {
				updateInviteProcessingStatus(member);
			}
		}
		collaborateService.updateMemberDisplayNameFromSSO(allMembers);
		return folderResponses;
	}

	private CollaborateMember updateInviteProcessingStatus(CollaborateMember member) {
		List<InviteProcessingStatus> inviteStatuses = collaborateDao.getInviteProcessingStatus(member.getMemberId());
		if (inviteStatuses == null || inviteStatuses.isEmpty()) {
			return member;
		}
		if (member.getCellphones() != null) {
			for (CellPhone cellphone : member.getCellphones()) {
				for (InviteProcessingStatus inviteStatus : inviteStatuses) {
					if (inviteStatus.isCellphoneEquals(cellphone)) {
						cellphone.setSmsStatusCode(inviteStatus.toStatusCode());
						break;
					}
				}
			}
		}
		if (member.getMails() != null) {
			for (Mail mail : member.getMails()) {
				for (InviteProcessingStatus inviteStatus : inviteStatuses) {
					if (inviteStatus.isMailEquals(mail)) {
						mail.setMailStatusCode(inviteStatus.toStatusCode());
						break;
					}
				}
			}
		}
		return member;
	}

	@Override
	public void sortCollaborateInfoResponses(
			List<GetCollaborateInfoResponse> collaborateInfoResponses,
			SortType sortType) {
		Collections.sort(collaborateInfoResponses,
				metadataResponseComparatorFactory
						.generateCollaborateInfoResponseComparator(sortType));
	}

	@Override
	@Transactional
	public void recursiveCreateFolderIfNotExists(String fullPath) {
		String[] pathArray = fullPath.split("/");
		String userId = pathArray.length > 0 ? pathArray[0] : "";
		String path = pathArray.length > 1 ? fullPath
				.substring(userId.length() + 1) : "";
		recursiveCreateFolderIfNotExists(userId, path);
	}

	@Override
	@Transactional
	public void recursiveCreateFolderIfNotExists(String userId, String path) {
		Metadata folder = metadataDao.getMetadataByPath(userId, "", "");

		if (folder == null) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		String[] pathArray = path.split("/");
		String currentPath = userId;

		for (int i = 0; i < pathArray.length; i++) {
			currentPath += "/" + pathArray[i];
			folder = metadataDao.getMetadataByPath(currentPath, "", "");

			if (folder == null) {
				PathRequestBase request = new PathRequestBase();
				request.setUserId(userId);
				request.setPath(path);
				request.setSourcePath("");
				folderService.createFolder(request);
			}
		}
	}

	@Override
	@Transactional
	public void recursiveCopyAccordingToPath(Metadata sourceMetadata,
			String userId, String path) {
		String[] pathArray = path.split("/");
		String currentPath = new StringBuilder().append(userId).append("/")
				.append(pathArray[0]).toString();
		// start from second layer
		// path: exclude the member id
		// ex: root/first layer/second layer
		for (int i = 1; i < pathArray.length; i++) {
			currentPath += "/" + pathArray[i];
			Metadata folder = metadataDao
					.getMetadataByPath(currentPath, "", "");

			if (folder == null) {
				PathRequestBase request = new PathRequestBase();
				request.setUserId(userId);
				request.setPath(path);
				request.setSourcePath("");
				folderService.createFolder(request);
				folder = metadataDao.getMetadataByPath(currentPath, "", "");
				// update note and icon
				metadataDao.updateNote(folder.getId(),
						sourceMetadata.getNote(), DateUtils.nowUTCTimestamp(),
						sourceMetadata.getCreatedBy());
				metadataDao.updateMetadataIcon(folder.getId(),
						sourceMetadata.getIconBlockId(),
						sourceMetadata.getIconText(),
						sourceMetadata.getIconTextColor(),
						sourceMetadata.getIconTextStyle());
			}
		}
	}

	@Override
	public Metadata buildBase4Create(PathInfo pathInfo) {
		String name = FilenameUtils.getName(pathInfo.getFullOwnerPath());
		long nowUTCTime = DateUtils.nowUTCTimestamp();
		Metadata metadataObj = new Metadata();
		metadataObj.setId(StringUtils.getUUID());
		metadataObj.setName(name.toLowerCase());
		metadataObj.setFolder(false);
		metadataObj.setOriginName(name);
		metadataObj.setModifiedAt(nowUTCTime);
		metadataObj.setModifiedBy(pathInfo.getUserId());
		metadataObj.setBackupCount(-1);
		metadataObj.setDeviceUid(pathInfo.getDeviceUid());
		metadataObj.setFullSourcePath(pathInfo.getFullSourcePath());
		metadataObj.setNote("");
		metadataObj.setEncrypted(false);
		metadataObj.setCreatedAt(nowUTCTime);
		metadataObj.setCreatedBy(pathInfo.getUserId());
		metadataObj.setType(MetadataType.NORMAL.toString());
		metadataObj.setOwnerId(pathInfo.getOwnerId());
		return metadataObj;
	}

	public int calculateTotalSize(List<? extends BaseMetadata> metadataList) {
		int totalSize = 0;
		for (BaseMetadata m : metadataList) {
			totalSize += m.getSize();
		}
		return totalSize;
	}

	public List<String> getIds(List<? extends BaseMetadata> metadataList) {
		List<String> ids = new ArrayList<String>();
		for (BaseMetadata m : metadataList) {
			ids.add(m.getId());
		}
		return ids;
	}

	// @Override
	// public String generateNameForCopy(List<Metadata> subMetadatas, Metadata
	// metadata, boolean isFolder) {
	// if (null == subMetadatas) {
	// subMetadatas = new ArrayList<Metadata>();
	// }
	// Set<String> set = new HashSet<String>();
	// for (Metadata m : subMetadatas) {
	// set.add(m.getOriginName());
	// }
	// String originalName = metadata.getOriginName();
	// String baseName;
	// String ext;
	//
	// int index = originalName.indexOf(".");
	// if (!isFolder && index > -1) {
	// baseName = originalName.substring(0, index);
	// ext = originalName.substring(index);
	// } else {
	// baseName = originalName;
	// ext = "";
	// }
	// String newName;
	// for (int i = 1; i < 100; i++) {
	// newName = String.format("%s - Copy(%s)%s", baseName, i, ext);
	// if (!set.contains(newName)) {
	// return newName;
	// }
	// }
	// return null;
	// }

	@Override
	public String generateName(List<Metadata> subMetadatas, Metadata metadata,
			boolean isFolder, boolean isMove) {
		if (null == subMetadatas) {
			subMetadatas = new ArrayList<Metadata>();
		}
		Set<String> set = new HashSet<String>();
		for (Metadata m : subMetadatas) {
			set.add(m.getOriginName());
		}
		String originalName = metadata.getOriginName();
		String baseName;
		String ext;

		int index = originalName.indexOf(".");
		if (!isFolder && index > -1) {
			baseName = originalName.substring(0, index);
			ext = originalName.substring(index);
		} else {
			baseName = originalName;
			ext = "";
		}
		String newName;
		String formator = isMove ? "%s (%s)%s" : "%s - Copy(%s)%s";
		for (int i = 1; i < 100; i++) {
			newName = String.format(formator, baseName, i, ext);
			if (!set.contains(newName)) {
				return newName;
			}
		}
		return null;
	}
	
	@Override
	public boolean hasUnreadFile(String metadataIndexId, String userId) {
		if(metadataDao.getUnreadSubFileCount(metadataIndexId, userId) > 0) {
			return true;
		}
		
		for(String subfolderId : metadataDao.getFolderIdsByParentId(metadataIndexId, true)) {
			if(hasUnreadFile(subfolderId, userId)) {
				return true;
			}
		}
		
		return false;
	}

	@Override
	public void adjustItemStyleByGlobalItemStyle(Metadata metadata, String ownerId) {
		ItemStyle globalItemStyle = metadataDao.getGlobalItemStyle(ownerId);
		if (metadata.getItemTextColor() == null) {
			metadata.setItemTextColor(globalItemStyle.getItemTextColor());
		}
		if (metadata.getItemBgColor() == null) {
			metadata.setItemBgColor(globalItemStyle.getItemBgColor());
		}
		if (metadata.isItemTextBold() == null) {
			metadata.setItemTextBold(globalItemStyle.isItemTextBold());
		}
		if (metadata.isItemTextItalic() == null) {
			metadata.setItemTextItalic(globalItemStyle.isItemTextItalic());
		}
		if (metadata.isItemTextUnderline() == null) {
			metadata.setItemTextUnderline(globalItemStyle.isItemTextUnderline());
		}
	}
}
