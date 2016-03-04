package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import constants.GridType;
import constants.SortType;
import constants.ViewType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProfileProperty implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String userId;
	@JsonProperty("device_uid")
	private String deviceUid;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String metadataIndexId;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private long profileId;
	@JsonProperty("name")
	private String propertyName;
	@JsonProperty("value")
	private String propertyValue;

	public ProfileProperty() {
		super();
	}

	public SortType toSortType() {
		SortType result = null;
		if (propertyName.equals("sortby_type")) {
			try {
				result = SortType.parse(Integer.parseInt(propertyValue));
			} catch (Exception e) {
			}
		}
		return (result != null ? result : SortType.CREATED_AT);
	}

	public ViewType toViewType() {
		ViewType result = null;
		if (propertyName.equals("viewby_type")) {
			try {
				result = ViewType.parse(Integer.parseInt(propertyValue));
			} catch (Exception e) {
			}
		}
		return (result != null ? result : ViewType.UNDEFINED);
	}

	public GridType toGridType() {
		GridType result = null;
		if (propertyName.equals("gridby_type")) {
			try {
				result = GridType.parse(Integer.parseInt(propertyValue));
			} catch (Exception e) {
			}
		}
		return (result != null ? result : GridType.UNDEFINED);
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getDeviceUid() {
		return deviceUid;
	}

	public void setDeviceUid(String deviceUid) {
		this.deviceUid = deviceUid;
	}

	public String getMetadataIndexId() {
		return metadataIndexId;
	}

	public void setMetadataIndexId(String metadataIndexId) {
		this.metadataIndexId = metadataIndexId;
	}

	public long getProfileId() {
		return profileId;
	}

	public void setProfileId(long profileId) {
		this.profileId = profileId;
	}

	public String getPropertyName() {
		return propertyName;
	}

	public void setPropertyName(String propertyName) {
		this.propertyName = propertyName;
	}

	public String getPropertyValue() {
		return propertyValue;
	}

	public void setPropertyValue(String propertyValue) {
		this.propertyValue = propertyValue;
	}

}
