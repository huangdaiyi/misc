package model;

import java.util.Calendar;

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBAttribute;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBIgnore;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBIndexHashKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBIndexRangeKey;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTable;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@DynamoDBTable(tableName="activity_history") 
@JsonIgnoreProperties(ignoreUnknown = true)
public class ActiveHistory implements Comparable<ActiveHistory>{
	
	@SuppressWarnings("unused")
	private static final long serialVersionUID = 1L;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String id;
	private String path = "";
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String oldPath = "";
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String metadataId = "";
	@JsonProperty("last_edit_user_id")
	private String lastEditUser = "";
	@JsonProperty("last_edit_time")
	private String lastEditTime = "";
	@JsonProperty("change_reason")
	private String changeReason = "";
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String sharedRootId = "";
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String level = "";
	@JsonProperty("last_edit_user")
	private String lastEditUserName = "";
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String sharedUpperPath = "";
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String oldSharedUpperPath = "";
	@JsonProperty("photo_block_id")
	private String photoBlockId = "";

	@DynamoDBAttribute(attributeName = "shared_upper_path")
	public String getSharedUpperPath() {
		return sharedUpperPath;
	}

	public void setSharedUpperPath(String sharedUpperPath) {
		this.sharedUpperPath = sharedUpperPath;
	}

	@DynamoDBAttribute(attributeName = "old_shared_upper_path")
	public String getOldSharedUpperPath() {
		return oldSharedUpperPath;
	}

	public void setOldSharedUpperPath(String oldSharedUpperPath) {
		this.oldSharedUpperPath = oldSharedUpperPath;
	}

	@DynamoDBHashKey(attributeName="id")
	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	@DynamoDBAttribute(attributeName = "path")
	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	@DynamoDBAttribute(attributeName = "old_path")
	public String getOldPath() {
		return oldPath;
	}

	public void setOldPath(String oldPath) {
		this.oldPath = oldPath;
	}

	@DynamoDBAttribute(attributeName = "metadata_id")
	public String getMetadataId() {
		return metadataId;
	}

	public void setMetadataId(String metadataId) {
		this.metadataId = metadataId;
	}

	@DynamoDBAttribute(attributeName = "last_edit_user")
	public String getLastEditUser() {
		return lastEditUser;
	}

	public void setLastEditUser(String lastEditUser) {
		this.lastEditUser = lastEditUser;
	}

	@DynamoDBAttribute(attributeName = "last_edit_time")
	@DynamoDBIndexRangeKey(globalSecondaryIndexName = "shared_folder_index",attributeName = "last_edit_time")
	public String getLastEditTime() {
		return lastEditTime;
	}

	public void setLastEditTime(String lastEditTime) {
		this.lastEditTime = lastEditTime;
	}


	@DynamoDBAttribute(attributeName = "level")
	public String getLevel() {
		return level;
	}

	public void setLevel(String level) {
		this.level = level;
	}

	@DynamoDBAttribute(attributeName = "change_reason")
	public String getChangeReason() {
		return changeReason;
	}

	public void setChangeReason(String changeReason) {
		this.changeReason = changeReason;
	}

	@DynamoDBAttribute(attributeName = "shared_root_id")
	@DynamoDBIndexHashKey(globalSecondaryIndexName = "shared_folder_index",attributeName = "shared_root_id")
	public String getSharedRootId() {
		return sharedRootId;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}

	@DynamoDBIgnore
	public String getLastEditUserName() {
		return lastEditUserName;
	}

	public void setLastEditUserName(String lastEditUserName) {
		this.lastEditUserName = lastEditUserName;
	}

	public String getPhotoBlockId() {
		return photoBlockId;
	}

	public void setPhotoBlockId(String photoBlockId) {
		this.photoBlockId = photoBlockId;
	}

	@Override
	public int compareTo(ActiveHistory arg0) {
		String s1=this.lastEditTime;
		String s2=arg0.getLastEditTime();
		java.text.DateFormat df=new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		Calendar c1=Calendar.getInstance();
		Calendar c2=Calendar.getInstance();
		try
		{
			c1.setTime(df.parse(s1));
			c2.setTime(df.parse(s2));
		}catch(java.text.ParseException e){
			
		}
		return c1.compareTo(c2);
	}

	

}
