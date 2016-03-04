package constants;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum MetadataType {

	BACKUP("backup"),
	BACKUPCOMMON("backupcommon"),
	COMMON("common"),
	NORMAL("normal"),
	SHARE("share"),
	SHARECOMMON("sharecommon"),
	LINKFILE("linkfile"),
	REFLINK("reflink"),
	ROOT("root"),
	COMMUNICATIONCOMMON("communicationcommon");

	private String type;

	private MetadataType(String type) {
		this.type = type;
	}

	@JsonCreator
	public static MetadataType parse(String type) {
		for (MetadataType metadataType : MetadataType.values()) {
			if (metadataType.getType().equals(type)) {
				return metadataType;
			}
		}
		return MetadataType.NORMAL;
	}

	@JsonValue
	public String getType() {
		return type;
	}

	@Override
	public String toString() {
		return this.type;
	}

	public MetadataType toBackupType() {
		if (this == BACKUPCOMMON || this == COMMON || this == SHARECOMMON) {
			return BACKUPCOMMON;
		}
		return BACKUP;
	}

}
