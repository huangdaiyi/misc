package dao;

import java.util.List;

import model.BaseMetadata;
import model.Metadata;
import model.UserSetting;
import model.request.RestCommonRequest;
import model.response.GetUserResponse;

public interface UserDao {

	public GetUserResponse getUser(String userId);

	public int updateSetting(UserSetting uerSetting);

	public int createUser(GetUserResponse user);

	public List<BaseMetadata> getUserAllNode(String id);

	public int updateCollaborate(List<String> idList);

	public int deleteAllMobileFolders(String user);

	public int deleteMobileFolders(String user, String deviceUid);

	public List<Metadata> getOridinaryCommonFolderChild(
			RestCommonRequest request);

	public List<Metadata> getAllCommonFiles(RestCommonRequest request);

	public List<Metadata> getMyDocumentFileChild(RestCommonRequest request);

	public int deleteAllBackupData(String userId);

	public int deleteBackupInAttr(String userId);

	public List<Metadata> getSubLevel(String owner_id, List<String> parentId,
			List<String> name, boolean isBackUp);

	public void updateUserDiskSpace(String userId, long total);
}