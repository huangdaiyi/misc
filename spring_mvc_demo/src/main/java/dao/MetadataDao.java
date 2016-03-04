package dao;

import java.util.List;

import model.ItemStyle;
import model.BaseMetadata;
import model.Metadata;
import model.SharedRootMap;
import constants.MetadataType;

public interface MetadataDao {

	public Metadata createMetadata(Metadata metadata);

	public Metadata getMetadataByCriteria(String parentId, String name,
			String deviceUid, String fullSourcePath);

	public List<Metadata> getMetadatasByParentId(String parentId);

	public List<Metadata> getMetadatasByType(String ownerId, MetadataType type);

	public List<Metadata> getFoldersByParentId(String parentId);
	
	public Metadata getMetadataByRefId(String userId,String refId);
	
	public List<Metadata> getMetadataByRefIdOnly(String refId);

	public List<Metadata> getRefMetadataBySharedId(List<String> sharesIds);

	public List<Metadata> getAllFilesInLinkFolder(String fullSourcePath,
			boolean includeConmmonFoders);

	public List<Metadata> getAllFilesByDeviceUid(String deviceUid);

	public List<Metadata> batchCreateMetadata(List<Metadata> metadatas);

	public Metadata getMetadata(String id);

	public Metadata getMetadataByPath(String path, String deviceUid,
			String fullSourcePath);

	public String getRootMetadataId(String ownerId);

	public void deleteMetadata(String id);

	public void batchDeleteMetadata(List<String> idList);

	public void updateNote(String id, String note, long modifiedAt,
			String userId);

	public void updateNewName(String id, String newName, String newSourcePath);

	public void updateNewNameToReflink(String id, String newName, String newSourcePath);
	
	public void updateBackupCount(String id, Integer backupCount);

	public List<Metadata> getParents(String id);

	public Metadata updateFileMetadata(Metadata metadata);

	public List<BaseMetadata> getUserAllNodeByOwnerId(String ownerId);

	public List<Metadata> getOwnerMetadatasByExtensions(String ownerId,
			List<String> extensions);

	public String getFullPath(String id);

	public void batchCleanMetadataIcon(List<Metadata> metadatas);

	public void updateMetadataIcon(String id, String iconBlockId,
			String iconText, String iconTextColor, String iconTextStyle);

	public void batchUpdateMetadataParentId(List<Metadata> metadatas);

	public void updateImageWidthHeight(String id, int width, int height);

	public void updateImageSize(String id, long size);

	public void updateImageBlockId(String id, String blockId);

	public void updateImageModifiedAt(String id, long modifiedAt);

	public int getMetadataCount();

	public long countSubMetadatasByParentId(String parentId, boolean isFolder);

	public List<Metadata> batchGetMetadata(int startIndex, int endIndex);

	public void batchUpdateMetadata(List<Metadata> metadatas);

	public void updateSharedRootId(String metadataIndexId);

	public int getMaxSortPriority(String parentId);

	public void updateMetadataType(String metadataIndexId,
			MetadataType metadataType);

	public void updateSyncRootId(String metadataIndexId, String syncRootId);

	public Metadata getSyncAppMetadataByPath(String path, String deviceUid,
			String fullSourcePath);

	public List<Metadata> getMetadataByRelativePath(
			String startWithMetadataIndexId, String relativePath);

	public List<Metadata> getMetadataByRelativePath(
			String startWithMetadataIndexId, List<String> splittedRelativePath);

	public void updateLinkFile(String metadataIndexId, String deviceUid,
			String fullSourcePath);

	public List<Metadata> batchGetMetadata(String owner_id,
			List<String> parentId, List<String> name);

	public void renameLinkFolder(String oldFolderFullSourcePath,
			String newFolderFullSourcePath);

	public List<? extends BaseMetadata> getMetadatasByPath(
			String[] pathFragmenets, String deviceUid, String fullSourcePath,
			int leave);

	public void batchResetBackupCount(List<Metadata> resetBackupCountMetadatas);

	List<Metadata> getMetadataBySharedRootId(String sharedRootId,
			String searchKey);
	
	public List<BaseMetadata> getOwnerMetadatasByBlockId(String ownerId, String blockId);
	
	public List<BaseMetadata> getMetadatasBySearchingNameAndNote(String ownerId, String keyword);
	
	public void updateFileEncryption(String id, String blockId, boolean encrypted, long modifiedAt, String userId); 
	
	public void transformSyncToNormal(String id, boolean reserveSyncRootId);

	public void updateSubSharedRootId(List<SharedRootMap> sharedMaps);
	
	public List<String> getFolderIdsByParentId(String parentId, boolean excludeNoAnyFileFolder);
	
	public int getUnreadSubFileCount(String id, String userId);
	
	public void updateItemStyle(String id, String itemTextColor, String itemBgColor, 
			Boolean itemTextBold, Boolean itemTextItalic, Boolean itemTextUnderline);

	public void updateSubItemsStyle(String id, String itemTextColor, String itemBgColor, 
			Boolean itemTextBold, Boolean itemTextItalic, Boolean itemTextUnderline);

	public void updateGlobalItemStyle(String ownerId, String itemTextColor,
			String itemBgColor, Boolean itemTextBold, Boolean itemTextItalic,
			Boolean itemTextUnderline);

	public ItemStyle getGlobalItemStyle(String ownerId);
}
