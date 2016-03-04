package dao;

import java.util.List;
import java.util.Map;
import java.util.Set;

import model.ProfileProperty;

public interface ProfilePropertyDao {

	public long getProfileId(String userId, String deviceUid, String metadataIndexId);

	public ProfileProperty replaceProfileProperty(ProfileProperty profileProperty);
	
	public List<ProfileProperty> getProfileProperties(String userId, String metadataIndexId);

	public ProfileProperty getProfileProperty(String userId, String deviceUid, String metadataIndexId, String propertyName);

	public void deleteProfileAndProperties(String userId, String deviceUid, String metadataIndexId);

	public void createProfileWithPropertys(ProfileProperty property);

	public List<ProfileProperty> getMultiProfileProperties(String userId, Set<String> metadataIndexId);

	public void copyProfileProperty(String userId, Map<String, String> maps);

}
