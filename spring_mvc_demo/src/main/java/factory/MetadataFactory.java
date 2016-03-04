package factory;

import java.util.List;
import java.util.Map;

import model.BackupMetadata;
import model.BaseMetadata;
import model.Metadata;
import model.MetadataAttr;
import model.PageProfileProperty;
import model.PathInfo;
import model.response.FileResponse;
import model.response.FolderResponse;
import model.response.GetCollaborateInfoResponse;
import constants.SortType;

public interface MetadataFactory {
	
	public Metadata buildBase4Create(PathInfo pathInfo);

	public String generateNameForMoveCopy(BaseMetadata metadata, boolean isFolder, boolean isMove);
	
	public GetCollaborateInfoResponse toGetCollaborateInfoResponse(Metadata collaborate, String userId);

	public List<GetCollaborateInfoResponse> updateGetCollaborateInfoResponseAfterPaging(List<GetCollaborateInfoResponse> folderResponses, String userId);

	public FolderResponse toFolderResponse(BaseMetadata folder, boolean displayNote, Map<String, Object> appendOtherParams);

	public FolderResponse toFolderResponse(BaseMetadata folder, boolean displayNote, String parentPath);

	public FolderResponse toFolderResponse(BaseMetadata folder, boolean displayNote, String parentPath, Map<String, Object> appendOtherParams);

	public List<FolderResponse> toFolderResponses(List<? extends BaseMetadata> folders, boolean displayNote, String parentPath);

	public List<FolderResponse> updateFolderResponsesAfterPaging(List<FolderResponse> folderResponses, String userId, String ownerId);

	public FileResponse toFileResponse(BaseMetadata file, boolean displayNote, Map<String, Object> appendOtherParams);

	public FileResponse toFileResponse(BaseMetadata file, boolean displayNote, String parentPath);

	public FileResponse toFileResponse(BaseMetadata file, boolean displayNote, String parentPath, Map<String, Object> appendOtherParams);

	public List<FileResponse> toFileResponses(String token, List<? extends BaseMetadata> files, boolean displayNote, String parentPath);

	public List<FileResponse> updateFileResponsesAfterPaging(List<FileResponse> fileResponses, String userId, BaseMetadata metadata, String ownerId);

	public void sortCollaborateInfoResponses(List<GetCollaborateInfoResponse> collaborateInfoResponses, SortType sortType);

	public void sortFolderResponses(List<FolderResponse> folderResponses, SortType sortType);

	public void sortFileResponses(List<FileResponse> fileResponses, SortType sortType);
	
	public <T> List<T> pagingFolderResponses(List<T> folderResponses, int pageNumber, int itemsPerPage, int prefixCount);

	public <T> List<T> pagingFileResponses(List<T> fileResponses, int pageNumber, int itemsPerPage, int prefixCount);

	public PageProfileProperty computePageProfileProperty(PathInfo pathInfo, String metadataIndexId, String viewerDeviceUniqueId, SortType forceSortByType);

	public String getMetadataOriginalPath(BaseMetadata metadata);

	public String getMetadataRelativePath(BaseMetadata metadata, String viewerId);
	
	public String getMetadataRelativePath(String metadataId, String viewerId);
	
	public String getSharedUpperPath(String sharedRootId);
	
	public String getSharedUpperPath(Metadata sharedFolder);

	public List<Metadata> getAllSubFoldersAndFiles(Metadata metadata);
	
	public List<Metadata> getAllSubFolders(Metadata metadata);

	public List<Metadata> getAllUpperLevelMetadatas(Metadata metadata);

	public List<MetadataAttr> generateMetadataAttrsForUpdateParents(Metadata metadata, BackupMetadata backupMetadata, int foldersCount, int filesCount, long totalSize);

	public BaseMetadata getBaseMetadata(String userId, String path, String sourcePath, String ownerId, String sharedRootId,boolean isFolder);
	
	public BaseMetadata getFile(String userId, String path, String sourcePath, String ownerId, String sharedRootId);

	public BaseMetadata getFolder(String userId, String path, String ownerId, String sharedRootId);

	public BaseMetadata getBaseMetadataByPathInfo(PathInfo pathInfo);

	public BaseMetadata getFileByPathInfo(PathInfo pathInfo);

	public BaseMetadata getFolderByPathInfo(PathInfo pathInfo);

	public BaseMetadata getBaseMetadataById(String id, boolean isBackup);

	public BaseMetadata getBaseMetadataByPath(PathInfo pathInfo);

	public List<? extends BaseMetadata> getBaseMetadatasByParentId(String metadataIndexId, boolean isUnderMyBackupData, boolean includeInvisible);

	public List<Metadata> parseBaseMetadataToMetadata(List<BackupMetadata> baseMetadatas);
	
	public void recursiveCreateFolderIfNotExists(String fullPath);
	
	public void recursiveCreateFolderIfNotExists(String userId, String path);
	
	public void recursiveCopyAccordingToPath(Metadata sourceMetadata, String userId, String path);
	
	public int calculateTotalSize(List<? extends BaseMetadata> metadataList);
		
	public List<String> getIds(List<? extends BaseMetadata> metadataList);

	public String generateName(List<Metadata> tempTargetMetadatas, Metadata tempTarget, boolean folder,boolean isMove);

	public boolean hasUnreadFile(String metadataIndexId, String userId);
	
	public void adjustFileResponsesItemStyleByGlobalItemStyle(List<FileResponse> metadataList, String ownerId);
	
	public void adjustItemStyleByGlobalItemStyle(Metadata metadata, String ownerId);
}
