package model.request;

import java.io.Serializable;
import java.util.List;

import model.CellPhone;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SSOFindUserIdByCellphoneReq implements Serializable{
	private static final long serialVersionUID = 1;
	@JsonProperty("cellphones")
	private List<CellPhone> cellPhones;
	public List<CellPhone> getCellPhones() {
		return cellPhones;
	}
	public void setCellPhones(List<CellPhone> cellPhones) {
		this.cellPhones = cellPhones;
	}
	public SSOFindUserIdByCellphoneReq(List<CellPhone> cellPhones)
	{
		setCellPhones(cellPhones);
	}
}
