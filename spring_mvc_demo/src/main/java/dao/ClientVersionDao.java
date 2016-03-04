package dao;


import model.response.ClientVersionResponse;

public interface ClientVersionDao {
	
	public ClientVersionResponse getClientVersion(String platform);
	
	public void updateClientVersion(String platform, String version, String updateType, String downloadUrl);

}
