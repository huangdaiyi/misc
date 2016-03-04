package model;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.jivesoftware.smack.util.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import constants.SyncRelationAction;

public class SyncRelation implements Cloneable {
	
	private long id;
	
	// source
	
	private String ownerId;
	private String sourcePath;
	private String metadataIndexId;
	
	// target

	private String destinationOwnerId;
	private String destinationSourcePath;
	private String destinationMetadataIndexId;
	
	// other
	
	private SyncRelationAction action;
	private Boolean enable;
	
	
	
	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}

	@Deprecated
	public static RowMapper<SyncRelation> getMapper(){
		return new RowMapper<SyncRelation>() {
			@Override
			public SyncRelation mapRow(ResultSet rs, int rowNum) throws SQLException {
				SyncRelation syncRelation = new SyncRelation();
				syncRelation.setId(rs.getInt("id"));
				syncRelation.setOwnerId(rs.getString("owner_id"));
				syncRelation.setSourcePath(rs.getString("source_path"));
				syncRelation.setMetadataIndexId(rs.getString("metadata_index_id"));
				syncRelation.setDestinationOwnerId(rs.getString("destination_owner_id"));
				syncRelation.setDestinationSourcePath(rs.getString("destination_source_path"));
				syncRelation.setDestinationMetadataIndexId(rs.getString("destination_metadata_index_id"));
				syncRelation.setAction(SyncRelationAction.parse(rs.getString("action")));
				syncRelation.setEnable(rs.getBoolean("enable"));
				return syncRelation;
			}
		};
	}
	
	public boolean isSourceInClient() {
		return !StringUtils.isNullOrEmpty(sourcePath) && StringUtils.isNullOrEmpty(metadataIndexId);
	}
	
	public boolean isDestinationInClient() {
		return !StringUtils.isNullOrEmpty(destinationSourcePath) && StringUtils.isNullOrEmpty(destinationMetadataIndexId);
	}
	
	public boolean isCrossClientCloud() {
		return isSourceInClient() != isDestinationInClient();
	}
	
	public String getSourceFullSourcePath() {
		return StringUtils.isNullOrEmpty(sourcePath) ? "" : ownerId + "/" + sourcePath;
	}
	
	public String getDestinationFullSourcePath() {
		return StringUtils.isNullOrEmpty(destinationSourcePath) ? "" : destinationOwnerId + "/" + destinationSourcePath;
	}
	
	public String getSourceKey() {
		return isSourceInClient() ? getSourceFullSourcePath() : getMetadataIndexId();
	}
	
	public String getDestinationKey() {
		return isDestinationInClient() ? getDestinationFullSourcePath() : getDestinationMetadataIndexId();
	} 
	
	public String toRelationKey() {
		return (getSourceKey().compareTo(getDestinationKey()) > 0)?
				getSourceKey() + ":" + getDestinationKey():
				getDestinationKey() + ":" + getSourceKey();
	}
	
	public SyncRelation swapSourceTarget() {
		destinationOwnerId = swapString(ownerId, ownerId = destinationOwnerId);
		destinationSourcePath = swapString(sourcePath, sourcePath = destinationSourcePath);
		destinationMetadataIndexId = swapString(metadataIndexId, metadataIndexId = destinationMetadataIndexId);
		return this;
	}
	
	public SyncRelation createSwappedSyncRelation() {
		try {
			SyncRelation syncRelation = (SyncRelation)this.clone();
			syncRelation.swapSourceTarget();
			return syncRelation;
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}
	
	public String getDestinationOrSourceSyncRootId() {
		return !StringUtils.isNullOrEmpty(destinationMetadataIndexId) ? destinationMetadataIndexId : metadataIndexId;
	}
	
	
	
	/**
	 * This is a tricky approach.
	 * Usage: str2 = swapString(str1, str1 = str2);
	 * 
	 * @param str1 str1
	 * @param str2 str1 = str2
	 */
	private static String swapString(String str1 , String str2) {
		return str1;
	}



	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}

	public String getSourcePath() {
		return sourcePath;
	}

	public void setSourcePath(String sourcePath) {
		this.sourcePath = sourcePath;
	}

	public String getMetadataIndexId() {
		return metadataIndexId;
	}

	public void setMetadataIndexId(String metadataIndexId) {
		this.metadataIndexId = metadataIndexId;
	}

	public String getDestinationOwnerId() {
		return destinationOwnerId;
	}

	public void setDestinationOwnerId(String destinationOwnerId) {
		this.destinationOwnerId = destinationOwnerId;
	}

	public String getDestinationSourcePath() {
		return destinationSourcePath;
	}

	public void setDestinationSourcePath(String destinationSourcePath) {
		this.destinationSourcePath = destinationSourcePath;
	}

	public String getDestinationMetadataIndexId() {
		return destinationMetadataIndexId;
	}

	public void setDestinationMetadataIndexId(String destinationMetadataIndexId) {
		this.destinationMetadataIndexId = destinationMetadataIndexId;
	}

	public SyncRelationAction getAction() {
		return action;
	}

	public void setAction(SyncRelationAction action) {
		this.action = action;
	}

	public Boolean getEnable() {
		return enable;
	}

	public void setEnable(Boolean enable) {
		this.enable = enable;
	}

}
