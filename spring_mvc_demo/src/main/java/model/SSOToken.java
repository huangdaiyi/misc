package model;

public class SSOToken {

	private String token;
	private String userId;
	private String deviceId;

	public SSOToken() {
		super();
	}

	public RequestBase toRequestBase() {
		RequestBase requestBase = new RequestBase();
		requestBase.setToken(token);
		requestBase.setUserId(userId);
		requestBase.setDeviceId(deviceId);
		return requestBase;
	}

	public User toUser() {
		User user = new User();
		user.setUserId(this.userId);
		return user;
	}

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getDeviceId() {
		return deviceId;
	}

	public void setDeviceId(String deviceId) {
		this.deviceId = deviceId;
	}

}
