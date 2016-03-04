package model.request;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SSOFindCellphonesByUserIdsRequest implements Serializable {
	private static final long serialVersionUID = 1;
	
	@JsonProperty("user_ids")
	private List<String> userIds;

	public SSOFindCellphonesByUserIdsRequest() {
		super();
	}

	public List<String> getUserIds() {
		return userIds;
	}

	public void setUserIds(List<String> userIds) {
		this.userIds = userIds;
	}
	
}
