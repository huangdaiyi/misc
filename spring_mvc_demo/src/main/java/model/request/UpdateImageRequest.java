package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateImageRequest extends PathRequestBase {
	private static final long serialVersionUID = 1;
	
	private Integer width;
	private Integer height;
	private Long size;
	@JsonProperty("block_id")
	private String blockId;
	@JsonProperty("last_edit_time")
	private Long modifiedAt;
	@JsonProperty("create_backup")
	private Boolean createBackup = true;
	
	
	public Integer getWidth() {
		return width;
	}
	
	public void setWidth(Integer width) {
		this.width = width;
	}
	
	public Integer getHeight() {
		return height;
	}
	
	public void setHeight(Integer height) {
		this.height = height;
	}
	
	public Long getSize() {
		return size;
	}
	
	public void setSize(Long size) {
		this.size = size;
	}
	
	public String getBlockId() {
		return blockId;
	}
	
	public void setBlockId(String blockId) {
		this.blockId = blockId;
	}
	
	public Long getModifiedAt() {
		return modifiedAt;
	}
	
	public void setModifiedAt(Long modifiedAt) {
		this.modifiedAt = modifiedAt;
	}
	
	public Boolean getCreateBackup() {
		return createBackup;
	}
	
	public void setCreateBackup(Boolean createBackup) {
		this.createBackup = createBackup;
	}
}
