package model.request;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetGlobalIconRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("include_user_defined_icon")
	private boolean includeUserDefinedIcon = true;
	@JsonProperty("is_folder")
	private Boolean folder;
	@JsonProperty("group_name")
	private String groupName;

	

	public Boolean getFolder() {
		return folder;
	}

	public void setFolder(Boolean folder) {
		this.folder = folder;
	}

	public GetGlobalIconRequest() {
		super();
	}

	public boolean isIncludeUserDefinedIcon() {
		return includeUserDefinedIcon;
	}

	public void setIncludeUserDefinedIcon(boolean includeUserDefinedIcon) {
		this.includeUserDefinedIcon = includeUserDefinedIcon;
	}

	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}



}
