package utils;

import java.util.ArrayList;
import java.util.List;

import model.BackupMetadata;
import model.Metadata;
import constants.MetadataType;

public final class TypeConvert {
	public static List<BackupMetadata> convert(List<Metadata> metadatas) {
		List<BackupMetadata> list = new ArrayList<BackupMetadata>();
		for (Metadata item : metadatas) {
			BackupMetadata metadata = new BackupMetadata();
			metadata.setId(item.getId());
			metadata.setParentId(item.getParentId());
			metadata.setName(item.getName());
			metadata.setFolder(item.isFolder());
			metadata.setSortPriority(item.getSortPriority());
			metadata.setTotalSize(item.getTotalSize());
			metadata.setOriginName(item.getOriginName());
			metadata.setModifiedAt(item.getModifiedAt());
			metadata.setModifiedBy(item.getModifiedBy());
			metadata.setDeviceUid(item.getDeviceUid());
			metadata.setNote(item.getNote());
			metadata.setParams(item.getParams());
			metadata.setIconBlockId(item.getIconBlockId());
			metadata.setEncrypted(item.isEncrypted());
			metadata.setCreatedAt(item.getCreatedAt());
			metadata.setCreatedBy(item.getCreatedBy());
			metadata.setBlockId(item.getBlockId());
			metadata.setType(item.getType());
			metadata.setOwnerId(item.getOwnerId());
			list.add(metadata);
		}
		return list;
	}

	public static BackupMetadata convert(Metadata metadata, String id, String parentId){
		return convert(metadata, id , parentId, 1);
	}
	
	public static BackupMetadata convert(Metadata metadata, String id, String parentId, int backupNo) {

		BackupMetadata backupMetadata = new BackupMetadata();
		backupMetadata.fromBaseMetadata(metadata);
		
		backupMetadata.setId(StringUtils.isNullOrEmpty(id) ? StringUtils.getUUID() : id);
		backupMetadata.setBackupNo(backupNo); // the latest is 1
		backupMetadata.setOriginalIndexId(metadata.getId());
		backupMetadata.setParentId(StringUtils.isNullOrEmpty(parentId) ? StringUtils.getUUID() : parentId);
		backupMetadata.setType(MetadataType.parse(metadata.getType()).toBackupType().toString());

		return backupMetadata;
	}
}
