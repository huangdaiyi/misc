package model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;


@JsonIgnoreProperties(ignoreUnknown = true)
public class AlreadyRead implements Serializable {
	private static final long serialVersionUID = 1L;

	private String metadataIndexId;
	private String userId;
	private List<CellPhone> cellphone = new ArrayList<CellPhone>();

	public AlreadyRead() {
		super();
	}

	public String getMetadataIndexId() {
		return metadataIndexId;
	}

	public void setMetadataIndexId(String metadataIndexId) {
		this.metadataIndexId = metadataIndexId;
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public List<CellPhone> getCellphone() {
		return cellphone;
	}

	public void setCellphone(List<CellPhone> cellphone) {
		this.cellphone = cellphone;
	}
	
}