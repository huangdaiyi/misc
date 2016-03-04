package service;

import java.util.Collection;
import java.util.List;

import model.BaseMetadata;
import model.Metadata;
import model.PathRequestBase;
import model.RequestBase;
import model.SyncRelation;
import model.request.SyncRelationPathRequest;
import model.request.SyncRelationRequest;
import model.response.SyncRelationResponse;
import constants.SystemEvent;
import exception.MetadataException;

public interface SyncRelationService {

	public void addSyncRelation(SyncRelationRequest syncRelationRequest)
			throws MetadataException;
	
	public List<SyncRelationResponse> querySyncRelation(SyncRelationPathRequest syncRelationPathRequest)
			throws MetadataException;
	
	public void updateSyncRelation(SyncRelationRequest syncRelationRequest)
			throws MetadataException;
	
	public void deleteSyncRelations(String userId, List<SyncRelation> syncRelations)
			throws MetadataException;
	
	public void deleteSyncRelation(SyncRelationRequest syncRelationRequest)
			throws MetadataException;
	
	public void deleteRelatedSyncRelation(String syncRootId, String userId)
			throws MetadataException;
	
	public void sync(BaseMetadata metadata, SystemEvent operation)
			throws MetadataException;
	
	public void sync(RequestBase request, BaseMetadata metadata, SystemEvent operation)
			throws MetadataException;
	
	public void transformNormalToSync(SyncRelationRequest syncRelationRequest)
			throws MetadataException;
	
	public void transformSyncToNormal(PathRequestBase pathRequest)
			throws MetadataException;
	
	public boolean isCaller();
	
	public boolean inCallerList();
	
	public void cleanSynRelationRelatedEmptyFolders(String userId, SyncRelation syncRelation);
	
	public boolean hasSyncFolder(Metadata folder);
	
	public boolean hasSyncFolder(Metadata folder, boolean selfIncluded);
	
	public boolean hasSyncFolder(Metadata folder, boolean selfIncluded, Collection<Metadata> subFolders);
	
	public List<String> getAllSubSyncFolderId(BaseMetadata folder, boolean selfIncluded, Collection<? extends BaseMetadata> subFolders);
}
