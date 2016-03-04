package model.response;

import java.io.Serializable;
import java.util.List;

import model.CellPhone;
import utils.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SSOFindUserIdByCellphone implements Serializable{
	private static final long serialVersionUID = 1;
	private List<CellPhone> users;
	public List<CellPhone> getUsers() {
		return users;
	}
	public void setUsers(List<CellPhone> users) {
		this.users = users;
	}
	public boolean checkUsersExists(){
		for (CellPhone user : users) {
			if(!StringUtils.isNullOrEmpty(user.getUserId())){
				return true;
			}
		}
		return false;
	}

}

