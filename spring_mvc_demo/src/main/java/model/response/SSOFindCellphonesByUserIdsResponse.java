package model.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SSOFindCellphonesByUserIdsResponse implements Serializable {
	private static final long serialVersionUID = 1;
	
	private List<SSOFindCellphonesByUserIdResponse> users;
	
	public SSOFindCellphonesByUserIdsResponse() {
		super();
	}

	public List<SSOFindCellphonesByUserIdResponse> getUsers() {
		return users;
	}

	public void setUsers(List<SSOFindCellphonesByUserIdResponse> users) {
		this.users = users;
	}
	
}
