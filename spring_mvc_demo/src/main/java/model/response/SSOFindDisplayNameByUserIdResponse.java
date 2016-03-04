package model.response;

import java.io.Serializable;
import java.util.List;

public class SSOFindDisplayNameByUserIdResponse implements Serializable {
	private static final long serialVersionUID = 1;

	private String status;
	private List<String> userinfo;
	
	public SSOFindDisplayNameByUserIdResponse() {
		super();
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public List<String> getUserinfo() {
		return userinfo;
	}

	public void setUserinfo(List<String> userinfo) {
		this.userinfo = userinfo;
	}
}
