package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GlobalIcon implements Serializable {
	private static final long serialVersionUID = 1L;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private Integer id;
	@JsonProperty("block_id")
	private String blockId;
	@JsonProperty("is_default")
	private Boolean defaultIcon;
	@JsonProperty("user_id")
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String user;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String type;
	@JsonProperty("is_system")
	private Boolean system;
	@JsonProperty("is_folder")
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private Boolean folder;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private Integer groupId;

	public GlobalIcon() {
		super();
	}

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getBlockId() {
		return blockId;
	}

	public void setBlockId(String blockId) {
		this.blockId = blockId;
	}

	public Boolean getDefaultIcon() {
		return defaultIcon;
	}

	public void setDefaultIcon(Boolean defaultIcon) {
		this.defaultIcon = defaultIcon;
	}

	public String getUser() {
		return user;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Boolean getSystem() {
		return system;
	}

	public void setSystem(Boolean system) {
		this.system = system;
	}

	public Boolean getFolder() {
		return folder;
	}

	public void setFolder(Boolean folder) {
		this.folder = folder;
	}

	public Integer getGroupId() {
		return groupId;
	}

	public void setGroupId(Integer groupId) {
		this.groupId = groupId;
	}
	

	
}