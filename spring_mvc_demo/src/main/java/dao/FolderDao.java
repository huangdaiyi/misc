package dao;

import java.util.List;

import model.GlobalIcon;
import model.GlobalIconGroup;
import model.Metadata;
import model.MobileFolders;
import model.response.GetSubBackupCountResponse;
import constants.SyncType;

public interface FolderDao {

	public String getFolderIdByCriteria(String parentId, String name,
			String deviceUid, String fullSourcePath);

	public void setFolderLinked(String user, String fullSourcePath);

	public void setFolderUnlinked(String user, String fullSourcePath);

	public List<MobileFolders> getQueryFolders(String user, SyncType type,
			String fullSourcePath);

	public void deleteMobileFolderByIds(List<Integer> idList);

	public void addGlobalIcon(List<GlobalIcon> globalicons);

	public void deleteGlobalIcon(List<GlobalIcon> globalIcons);

	public List<GlobalIcon> getGlobalIconByUser(String userId);

	public Integer getGlobalIconGroupId(String userId, boolean isFolder,
			String groupName);

	public List<GlobalIcon> getGlobalIcon(String userId,
			boolean includeUserDefinedIcon, boolean isFolder, int groupId);

	public void moveGlobalIcon(int iconId, int destinationId);

	public List<GlobalIconGroup> getGlobalIconGroup(String userId,
			boolean isFolder);

	public void createGlobalIconGroup(String userId, String name,
			boolean isFolder);

	public void deleteGlobalIconGroup(int groupId);

	public void updateGlobalIconGroup(GlobalIconGroup globalIcongroup);

	public void updatePriority(Metadata metadata);

	public List<GetSubBackupCountResponse> getSubBackupCount(List<String> idList);

	public List<String> getSubElementId(String id, String name, String ownerId);

	public List<Metadata> getBySharedRootId(String sharedRootId);

}
