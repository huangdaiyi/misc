package model;

public interface IRequestBase {

	public String getToken();

	public void setToken(String token);

	public String getUserId();

	public void setUserId(String userId);

	public String getDeviceId();

	public void setDeviceId(String deviceId);

	public String getDbHost();

	public void setDbHost(String dbHost);

	public int getDbPort();

	public void setDbPort(int dbPort);

	public String getDbInstance();

	public void setDbInstance(String dbInstance);

	public String getDbServerName();

	public void setDbServerName(String dbServerName);
}
