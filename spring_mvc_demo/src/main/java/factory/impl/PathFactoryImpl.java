package factory.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;

import model.GlobalSyncApp;
import model.Metadata;
import model.PathInfo;
import model.PathRequestBase;

import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import service.CollaborateService;
import utils.StringUtils;
import constants.CommonFolders;
import constants.HttpStatus;
import dao.MetadataDao;
import dao.SyncAppDao;
import exception.MetadataException;
import factory.PathFactory;

@Component
public class PathFactoryImpl implements PathFactory {

	@Autowired
	private CollaborateService collaborateService;

	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private SyncAppDao syncAppDao;

	private Map<String, CommonFolders> commonFolderMapping = new HashMap<String, CommonFolders>();

	@PostConstruct
	private void init() {
		commonFolderMapping.put("csv", CommonFolders.MY_DOCUMENTS_EXCEL);
		commonFolderMapping.put("xls", CommonFolders.MY_DOCUMENTS_EXCEL);
		commonFolderMapping.put("xlsx", CommonFolders.MY_DOCUMENTS_EXCEL);
		commonFolderMapping.put("doc", CommonFolders.MY_DOCUMENTS_WORD);
		commonFolderMapping.put("docx", CommonFolders.MY_DOCUMENTS_WORD);
		commonFolderMapping.put("pdf", CommonFolders.MY_DOCUMENTS_PDF);
		commonFolderMapping.put("ppt", CommonFolders.MY_DOCUMENTS_PPT);
		commonFolderMapping.put("pptx", CommonFolders.MY_DOCUMENTS_PPT);
		commonFolderMapping.put("txt", CommonFolders.MY_DOCUMENTS_TEXT);
		commonFolderMapping.put("zip", CommonFolders.MY_DOCUMENTS_ZIP);
		commonFolderMapping.put("rar", CommonFolders.MY_DOCUMENTS_ZIP);
		commonFolderMapping.put("7z", CommonFolders.MY_DOCUMENTS_ZIP);

		commonFolderMapping.put("bmp", CommonFolders.MY_PICTURES);
		commonFolderMapping.put("gif", CommonFolders.MY_PICTURES);
		commonFolderMapping.put("jpeg", CommonFolders.MY_PICTURES);
		commonFolderMapping.put("jpg", CommonFolders.MY_PICTURES);
		commonFolderMapping.put("png", CommonFolders.MY_PICTURES);

		commonFolderMapping.put("3gp", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("mp4", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("mpeg", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("mpg", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("wmv", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("asf", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("avi", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("flv", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("mkv", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("mov", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("rm", CommonFolders.MY_VIDEO);
		commonFolderMapping.put("rmvb", CommonFolders.MY_VIDEO);

		commonFolderMapping.put("mp3", CommonFolders.MY_MUSIC);
		commonFolderMapping.put("wav", CommonFolders.MY_MUSIC);
		commonFolderMapping.put("wma", CommonFolders.MY_MUSIC);
		commonFolderMapping.put("3ga", CommonFolders.MY_MUSIC);
	}

	@Override
	public CommonFolders mappingToCommonFolderByExtension(String fullSourcePath) {
		if (fullSourcePath != null) {
			fullSourcePath = fullSourcePath.toLowerCase();
		}
		String extension = FilenameUtils.getExtension(fullSourcePath);
		return commonFolderMapping.get(extension);
	}

	/**
	 * <ul>
	 * <li>path: Aaa/Bbb.jpg (old: aaa/bbb.jpg)(normal)</li>
	 * <li>sourcePath: my device sync folders/deviceUid/DCIM</li>
	 * <li>originPath: userId/Aaa/Bbb.jpg (old #request.origin_path:
	 * Aaa/Bbb.jpg, old OriginPath: userId/Aaa/Bbb.jpg)</li>
	 * <li>fullSourcePath: userId/my device sync folders/deviceUid/Aaa/Bbb.jpg</li>
	 * <li>path: Aaa/Bbb.jpg (old: aaa/bbb.jpg)(shared)</li>
	 * <li>originPath: userId/my shared datas/Aaa/Bbb.jpg
	 * <li>ownerPath: ownerId/Aaa/Bbb.jpg</li>
	 * </ul>
	 */
	@Override
	public PathInfo parsePathInfo(PathRequestBase pathRequestBase, boolean isFolder, boolean isPathExist) {
		String lowerCasePath = pathRequestBase.getPath().toLowerCase();
		PathInfo pathInfo = null;
		if (lowerCasePath.equals(CommonFolders.MY_DEVICE_SYNC_FOLDERS.toString()) || lowerCasePath.startsWith(CommonFolders.MY_DEVICE_SYNC_FOLDERS.toString().concat("/"))) {
			pathInfo = parseDevicePathInfo(pathRequestBase, isFolder);
		} else if (lowerCasePath.equals(CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString()) || lowerCasePath.startsWith(CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString().concat("/"))) {
			pathInfo = parseAppPathInfo(pathRequestBase);
		} else if (lowerCasePath.startsWith(CommonFolders.MY_SHARED_FOLDERS.toString().concat("/"))) {
			pathInfo = parseSharedFoldersPathInfo(pathRequestBase,isPathExist);
		}  else {
			pathInfo = parseNormalPathInfo(pathRequestBase,isPathExist);
		}
		pathInfo.setFolder(isFolder);
		return pathInfo;
	}

	private PathInfo parseNormalPathInfo(PathRequestBase pathRequestBase,boolean isPathExist) {
		String originFullPath = pathRequestBase.getUserId();
		if (pathRequestBase.getPath() != null && pathRequestBase.getPath().isEmpty() == false) {
			originFullPath = String.format("%s/%s", pathRequestBase.getUserId(), pathRequestBase.getPath());
		}
		PathInfo pathInfo = new PathInfo(pathRequestBase.getUserId(), originFullPath, "", "", pathRequestBase.getUserId(), originFullPath, "");
		if (StringUtils.isNullOrEmpty(pathRequestBase.getSourcePath()) == false) {
			pathInfo.setDeviceUid(fetchDeviceUid(pathRequestBase.getSourcePath()));
			pathInfo.setFullSourcePath(String.format("%s/%s", pathRequestBase.getUserId(), pathRequestBase.getSourcePath()));
		}
		String fullOnwerPath = "";
		if (!pathInfo.isUnderMyBackupData()) {
			Metadata metadata = null;
			if (isPathExist) {
				metadata = metadataDao.getMetadataByPath(pathInfo.getFullOriginPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
			}else {
				String parentPath = FilenameUtils.getPathNoEndSeparator(pathInfo.getFullOriginPath());
				metadata = metadataDao.getMetadataByPath(parentPath, "", "");
			}
			if (metadata != null) {
				if (!StringUtils.isNullOrEmpty(metadata.getSharedRootId())) {
					if (!canAccessCollaborate(pathRequestBase,metadata)) {
						throw new MetadataException(HttpStatus.CANT_ACCESS_COLLABORATE);
					}
					pathInfo.setUnderSharedFolders(true);
					pathInfo.setSharedRootId(metadata.getSharedRootId());					
					if (!metadata.getOwnerId().equals(pathInfo.getUserId())) {
						pathInfo.setOwnerId(metadata.getOwnerId());
						fullOnwerPath = metadataDao.getFullPath(metadata.getId());
						if (!isPathExist) {
							fullOnwerPath = fullOnwerPath +"/"+FilenameUtils.getName(pathInfo.getFullOriginPath());
						}
						pathInfo.setFullOwnerPath(fullOnwerPath);
					}					
				}
				if (!StringUtils.isNullOrEmpty(metadata.getSyncRootId())) {
					pathInfo.setSyncRootId(metadata.getSyncRootId());
				}
			}
		}
		
		return pathInfo;
	}

	private PathInfo parseDevicePathInfo(PathRequestBase pathRequestBase, boolean isFolder) {
		String originFullPath = "";

		if(!isFolder) {
			String extension = FilenameUtils.getExtension(pathRequestBase.getPath()).toLowerCase();
			if (commonFolderMapping.containsKey(extension) == false) {
				throw new MetadataException(HttpStatus.UNSUPPORT_COMMON_FOLDER_FILE_EXTENSION);
			}
			String targetCommonFolderPath = commonFolderMapping.get(extension).toString();
			originFullPath = String.format("%s/%s/%s", pathRequestBase.getUserId(), targetCommonFolderPath, FilenameUtils.getName(pathRequestBase.getPath()));
		}
		
		PathInfo pathInfo = new PathInfo();
		pathInfo.setUserId(pathRequestBase.getUserId());
		pathInfo.setFullOriginPath(originFullPath);
		pathInfo.setDeviceUid(fetchDeviceUid(pathRequestBase.getPath()));
		pathInfo.setFullSourcePath(String.format("%s/%s", pathRequestBase.getUserId(), pathRequestBase.getPath()));
		pathInfo.setOwnerId(pathRequestBase.getUserId());
		pathInfo.setFullOwnerPath(originFullPath);
		return pathInfo;
	}


	private PathInfo parseAppPathInfo(PathRequestBase pathRequestBase) {
		String originFullPath = "";
		PathInfo pathInfo = new PathInfo();
		String fromPath = "";
		if (pathRequestBase.getSourcePath() != null && !pathRequestBase.getSourcePath().equals("")) {
			pathInfo.setDeviceUid(fetchDeviceUid(pathRequestBase.getSourcePath()));
			fromPath = pathRequestBase.getSourcePath();
			originFullPath = String.format("%s/%s", pathRequestBase.getUserId(), pathRequestBase.getPath());
			pathInfo.setFullSourcePath(String.format("%s/%s", pathRequestBase.getUserId(), pathRequestBase.getSourcePath()));
		} else {
			pathInfo.setDeviceUid(fetchDeviceUid(pathRequestBase.getPath()));
			pathInfo.setFullSourcePath(String.format("%s/%s", pathRequestBase.getUserId(), pathRequestBase.getPath()));
			fromPath = pathRequestBase.getPath();
		}
		String[] sourcePathFragments = fromPath.split("/");
		StringBuilder appPath = new StringBuilder();
		if (sourcePathFragments.length >= 2) {
			for (int i = 2; i < sourcePathFragments.length - 1; i++) {
				appPath.append(sourcePathFragments[i]);
				if (i < sourcePathFragments.length - 2) {
					appPath.append("/");
				}
			}
		}
		List<GlobalSyncApp> globalSyncApps = syncAppDao.getUserDefinedAppByIntersection(pathRequestBase.getUserId(), pathInfo.getDeviceUid());
		for (GlobalSyncApp globalSyncApp : globalSyncApps) {
			if (globalSyncApp.getSingleFolderPath().equals(appPath.toString())) {
				originFullPath = String.format("%s/%s/%s/%s/%s", pathRequestBase.getUserId(), sourcePathFragments[0], globalSyncApp.getAppName().toLowerCase(), FilenameUtils.getName(globalSyncApp.getSingleFolderPath()),
						FilenameUtils.getName(pathRequestBase.getPath()));
				break;
			}
		}
		pathInfo.setUserId(pathRequestBase.getUserId());
		pathInfo.setFullOriginPath(originFullPath);
		pathInfo.setOwnerId(pathRequestBase.getUserId());
		pathInfo.setFullOwnerPath(originFullPath);
		return pathInfo;
	}

	private PathInfo parseSharedFoldersPathInfo(PathRequestBase pathRequestBase,boolean isPathExist) {
		String originFullPath = pathRequestBase.getUserId();
		if (pathRequestBase.getPath() != null && pathRequestBase.getPath().isEmpty() == false) {
			originFullPath = String.format("%s/%s", pathRequestBase.getUserId(), pathRequestBase.getPath());
		}
		PathInfo pathInfo = new PathInfo(pathRequestBase.getUserId(), originFullPath, "", "", pathRequestBase.getUserId(), originFullPath, "");
		if (StringUtils.isNullOrEmpty(pathRequestBase.getSourcePath()) == false) {
			pathInfo.setDeviceUid(fetchDeviceUid(pathRequestBase.getSourcePath()));
			pathInfo.setFullSourcePath(String.format("%s/%s", pathRequestBase.getUserId(), pathRequestBase.getSourcePath()));
		}
		String ownerId = pathRequestBase.fetchOwnerId();
		String sharedFolderPath  = isPathExist? pathInfo.getFullOriginPath():FilenameUtils.getFullPathNoEndSeparator(pathInfo.getFullOriginPath());
		if (collaborateService.canAccessCollaborate(pathRequestBase.getSharedRootId(), ownerId, pathInfo.getUserId(),sharedFolderPath)) {
			Metadata refMetadata = metadataDao.getMetadataByRefId(pathRequestBase.getUserId(), pathRequestBase.getSharedRootId());
			if (refMetadata != null) {
				throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
			}
			pathInfo.setUnderSharedFolders(true);
			if (!pathInfo.getUserId().equals(ownerId)) {
				String fullOwnerSharedRootPath = metadataDao.getFullPath(pathRequestBase.getSharedRootId());
				String requestSharedFolderPath = StringUtils.trimFilePathRoot(StringUtils.trimFilePathRoot(pathInfo.getFullOwnerPath()));
				pathInfo.setFullOwnerPath(FilenameUtils.getFullPath(fullOwnerSharedRootPath) + requestSharedFolderPath);
				pathInfo.setOwnerId(ownerId);
			}
			pathInfo.setSharedRootId(pathRequestBase.getSharedRootId());
		}
		return pathInfo;
	}

	/**
	 * my device sync folders/deviceUid/DCIM/xxx.jpg to deviceUid my sync app
	 * backup data/deviceUid/AppName/appLastFolderName/xxx.jpg to deviceUid
	 */
	@Override
	public String fetchDeviceUid(String sourcePath) {
		if (sourcePath != null) {
			String[] sourcePathFragments = sourcePath.split("/");
			if (sourcePathFragments.length >= 2 && (CommonFolders.MY_DEVICE_SYNC_FOLDERS.toString().equals(sourcePathFragments[0].toLowerCase()))
					|| (CommonFolders.MY_SYNC_APP_BACKUP_DATA.toString().equals(sourcePathFragments[0].toLowerCase()))) {
				return sourcePathFragments[1];
			}
		}
		return "";
	}

	public boolean isStreamingFile(String name) {
		CommonFolders mappingFolder = mappingToCommonFolderByExtension(name);
		return (mappingFolder == CommonFolders.MY_VIDEO || mappingFolder == CommonFolders.MY_MUSIC);
	}

	public boolean isReaderFile(String name) {
		CommonFolders mappingFolder = mappingToCommonFolderByExtension(name);
		return (mappingFolder != null && mappingFolder.toString().startsWith(CommonFolders.MY_DOCUMENTS.toString()));
	}

	public boolean isPicture(String name) {
		CommonFolders mappingFolder = mappingToCommonFolderByExtension(name);
		return (mappingFolder == CommonFolders.MY_PICTURES);
	}

	/**
	 * forbidden: 1. under common folders(except MY_SHARED_FOLDERS & sub-folder
	 * of MY_COMMUNICATION_DATA) 2. sub-file of MY_SHARED_FOLDERS
	 */
	@Override
	public void checkCanCreateFile(String path) {
		String lowerCasePath = path.toLowerCase();
		for (CommonFolders common : CommonFolders.values()) {
			if (common == CommonFolders.My_STORAGE_DATA)
				continue;

			if (lowerCasePath.startsWith(common.toString().concat("/"))) {
				switch (common) {
				case MY_SHARED_FOLDERS:
				case MY_COMMUNICATION_DATA:
					if (lowerCasePath.split("/").length <= 2) {
						throw new MetadataException(HttpStatus.CANT_CREATE_FILE_IN_THIS_FOLDER);
					}
					break;

				case MY_DEVICE_SYNC_FOLDERS:
				case MY_SYNC_APP_BACKUP_DATA:
				case MY_COMMUNICATION_DATA_CHAT:
				case MY_COMMUNICATION_DATA_CONTACT:
					break; // Ignore

				default:
					throw new MetadataException(HttpStatus.CANT_CREATE_FOLDER_IN_THIS_FOLDER);
				}
			}
		}
	}

	/**
	 * forbidden: 1. backup file
	 */
	@Override
	public void checkCanMoveFileFrom(String path) {
		if (path.toLowerCase().startsWith(CommonFolders.MY_BACKUP_DATA.toString().concat("/"))) {
			throw new MetadataException(HttpStatus.CANT_MOVE_FILE);
		}
	}

	/**
	 * forbidden: 1. common folders 2. under common folders(except
	 * MY_SHARED_FOLDERS)
	 */
	@Override
	public void checkCanMoveCopyFileTo(String path) {
		String lowerCasePath = path.toLowerCase();
		for (CommonFolders common : CommonFolders.values()) {
			if (common == CommonFolders.My_STORAGE_DATA) {
				continue;
			}
			if (lowerCasePath.equals(common.toString()) || lowerCasePath.startsWith(common.toString().concat("/"))) {
				if (common.equals(CommonFolders.MY_SHARED_FOLDERS)) {
					if (lowerCasePath.split("/").length == 1) {
						throw new MetadataException(HttpStatus.CANT_CREATE_FILE_IN_THIS_FOLDER);
					}
				} else {
					throw new MetadataException(HttpStatus.CANT_CREATE_FILE_IN_THIS_FOLDER);
				}
			}
		}
	}

	/**
	 * forbidden: 1. common folders 2. under common folders(except
	 * MY_SHARED_FOLDERS & sub-folder of MY_COMMUNICATION_DATA) 3. sub-folder of
	 * MY_SHARED_FOLDERS
	 */
	@Override
	public void checkCanCreateFolder(String path) {
		String lowerCasePath = path.toLowerCase();
		for (CommonFolders common : CommonFolders.values()) {
			if (common == CommonFolders.My_STORAGE_DATA)
				continue;

			if (lowerCasePath.equals(common.toString()) || lowerCasePath.startsWith(common.toString().concat("/"))) {
				switch (common) {

				case MY_SHARED_FOLDERS:
				case MY_COMMUNICATION_DATA:
					if (lowerCasePath.split("/").length <= 2) {
						throw new MetadataException(HttpStatus.CANT_CREATE_FOLDER_IN_THIS_FOLDER);
					}
					break;

				case MY_COMMUNICATION_DATA_CHAT:
				case MY_COMMUNICATION_DATA_CONTACT:
					break; // Ignore

				default:
					throw new MetadataException(HttpStatus.CANT_CREATE_FOLDER_IN_THIS_FOLDER);
				}
			}
		}
	}

	/**
	 * forbidden: 1. MY_DEVICE_SYNC_FOLDERS or under it
	 */
	@Override
	public void checkCanNoteFolder(String path) {
		String lowerCasePath = path.toLowerCase();
		if (lowerCasePath.equals(CommonFolders.MY_DEVICE_SYNC_FOLDERS.toString()) || lowerCasePath.startsWith(CommonFolders.MY_DEVICE_SYNC_FOLDERS.toString().concat("/"))) {
			throw new MetadataException(HttpStatus.CANT_NOTE_FOLDER);
		}
	}

	/**
	 * forbidden: 1. common folders 2. under common folders(except
	 * MY_SHARED_FOLDERS) 3. sub-folder of MY_SHARED_FOLDERS
	 */
	@Override
	public void checkCanMoveFolderFrom(String path) {
		checkCanCreateFolder(path);
	}

	/**
	 * forbidden: 1. common folders 2. under common folders(except
	 * MY_SHARED_FOLDERS)
	 */
	@Override
	public void checkCanMoveFolderTo(String path) {
		String lowerCasePath = path.toLowerCase();
		for (CommonFolders common : CommonFolders.values()) {
			if (common == CommonFolders.My_STORAGE_DATA || common == CommonFolders.MY_SHARED_FOLDERS) {
				continue;
			}
			if (lowerCasePath.equals(common.toString()) || lowerCasePath.startsWith(common.toString().concat("/"))) {
				throw new MetadataException(HttpStatus.CANT_MOVE_FOLDER_TO);
			}
		}
	}

	/**
	 * To Be Path and Override forbidden: 1. common folders
	 */
	@Override
	public void checkCanMoveFolderToBeOverride(String toBePath) {
		String lowerCasePath = toBePath.toLowerCase();
		for (CommonFolders common : CommonFolders.values()) {
			if (common == CommonFolders.My_STORAGE_DATA)
				continue;

			if (lowerCasePath.equals(common.toString())) {
				throw new MetadataException(HttpStatus.CANT_CREATE_FOLDER_IN_THIS_FOLDER);
			}
		}
	}

	/**
	 * forbidden: 1. MY_SHARED_FOLDERS
	 */
	@Override
	public void checkCanCopyFolderFrom(String path) {
		String lowerCasePath = path.toLowerCase();
		if (lowerCasePath.equals(CommonFolders.MY_SHARED_FOLDERS.toString())) {
			throw new MetadataException(HttpStatus.CANT_COPY_FOLDER_FROM);
		}
	}

	/**
	 * forbidden: 1. common folders 2. under common folders(except
	 * MY_SHARED_FOLDERS)
	 */
	@Override
	public void checkCanCopyFolderTo(String path) {
		checkCanMoveCopyFileTo(path);
	}

	/**
	 * To Be Path and Override forbidden: 1. common folders
	 */
	@Override
	public void checkCanCopyFolderToBeOverride(String toBePath) {
		checkCanMoveFolderToBeOverride(toBePath);
	}

	/**
	 * allowed: under MY_DEVICE_SYNC_FOLDERS/deviceUid/
	 */
	@Override
	public void checkCanLinkFolder(String path) {
		Boolean allowed = path.toLowerCase().startsWith(CommonFolders.MY_DEVICE_SYNC_FOLDERS.toString().toLowerCase()) && path.split("/").length > 2;
		if (!allowed) {
			throw new MetadataException(HttpStatus.CANT_LINK_FOLDER);
		}
	}

	/**
	 * same as checkCanLinkFolder() allowed: under
	 * MY_DEVICE_SYNC_FOLDERS/deviceUid/
	 */
	@Override
	public void checkCanUnlinkFolder(String path) {
		try {
			checkCanLinkFolder(path);
		} catch (Exception e) {
			throw new MetadataException(HttpStatus.CANT_UNLINK_FOLDER);
		}
	}

	/**
	 * forbidden: 1. common folders
	 */
	@Override
	public void checkCanZipUnzip(String path) {
		String lowerCasePath = path.toLowerCase();
		for (CommonFolders common : CommonFolders.values()) {
			if (common == CommonFolders.My_STORAGE_DATA)
				continue;

			if (common != CommonFolders.MY_SHARED_FOLDERS && (lowerCasePath.equals(common.toString()) || lowerCasePath.startsWith(common.toString().concat("/")))) {
				throw new MetadataException(HttpStatus.CANT_ZIP_UNZIP);
			}
		}
	}

	@Override
	public void checkCanCreateCollaborate(String path) {
		String lowerCasePath = path.toLowerCase();
		for (CommonFolders common : CommonFolders.values()) {
			if (common.toString().equals(CommonFolders.MY_COMMUNICATION_DATA.toString())
					|| common.toString().startsWith(CommonFolders.MY_COMMUNICATION_DATA.toString().concat("/"))
					|| common == CommonFolders.My_STORAGE_DATA) {
				continue;
			}
			if (lowerCasePath.equals(common.toString()) || lowerCasePath.startsWith(common.toString().concat("/"))) {
				throw new MetadataException(HttpStatus.CANT_CREATE_FOLDER_IN_THIS_FOLDER);
			}
		}
	}
	private boolean canAccessCollaborate(PathRequestBase pathRequestBase,Metadata sharedmMetadata){
		boolean check = false;
		if (StringUtils.isNullOrEmpty(pathRequestBase.getOwnerId())
				||StringUtils.isNullOrEmpty(pathRequestBase.getSharedRootId())) {
			if (!sharedmMetadata.getOwnerId().equals(pathRequestBase.getUserId())) {
				return check ;
			}
		}else{
			if (!sharedmMetadata.getOwnerId().equals(pathRequestBase.getOwnerId())
					||!sharedmMetadata.getSharedRootId().equals(pathRequestBase.getSharedRootId())) {
				return check;			
			}
		}
		String ownerId = pathRequestBase.fetchOwnerId();
		check = collaborateService.canAccessCollaborate(sharedmMetadata.getSharedRootId(), ownerId, pathRequestBase.getUserId(), "");
		return check;
	}
}
