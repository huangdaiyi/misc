package model.response;

import java.io.Serializable;
import java.util.List;

import model.CellPhone;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SSOFindCellphonesByUserIdResponse implements Serializable {
	private static final long serialVersionUID = 1;
	
	@JsonProperty("user_id")
	private String userId;
	private List<CellPhone> cellphones;
	
	public SSOFindCellphonesByUserIdResponse() {
		super();
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public List<CellPhone> getCellphones() {
		return cellphones;
	}

	public void setCellphones(List<CellPhone> cellphones) {
		this.cellphones = cellphones;
	}
	
}
