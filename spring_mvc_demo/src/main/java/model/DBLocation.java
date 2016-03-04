package model;

public class DBLocation {
	private String userId;
	private String host;
	private int port;
	private String serverName;
	private String dbInstance;
	
	public DBLocation(){
	}
	public DBLocation(String userId, String host, int port, String serverName, String dbInstance){
		this.userId = userId;
		this.host = host;
		this.port = port;
		this.serverName = serverName;
		this.dbInstance = dbInstance;
	}
	
	public String getUserId(){
		return this.userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}
	
	public String getHost(){
		return this.host;
	}
	public void setHost(String host){
		this.host = host;
	}
	
	public int getPort(){
		return this.port;
	}
	public void setPort(int port){
		this.port = port;
	}

	public String getServerName() {
		return this.serverName;
	}

	public void setServerName(String serverName) {
		this.serverName = serverName;
	}
	
	public String getDbInstance(){
		return this.dbInstance;
	}
	public void setDbInstance(String dbInstance){
		this.dbInstance = dbInstance;
	}
}
