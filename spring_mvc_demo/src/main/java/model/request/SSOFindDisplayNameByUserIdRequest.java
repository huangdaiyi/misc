package model.request;

import java.io.Serializable;
import java.util.List;

public class SSOFindDisplayNameByUserIdRequest implements Serializable {
	private static final long serialVersionUID = 1;
	
	private List<String> users;
	
	public SSOFindDisplayNameByUserIdRequest() {
		super();
	}
	
	public List<String> getUsers() {
		return users;
	}

	public void setUsers(List<String> users) {
		this.users = users;
	}
}
