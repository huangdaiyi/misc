package model;

public class BackupMetadata4Delete {

	private String originalIndexId;
	private int backupNo;

	public String getOriginalIndexId() {
		return originalIndexId;
	}

	public BackupMetadata4Delete(String originalIndexId, int backupNo) {
		this.originalIndexId = originalIndexId;
		this.backupNo = backupNo;
	}

	public BackupMetadata4Delete() {
	}

	public void setOriginalIndexId(String originalIndexId) {
		this.originalIndexId = originalIndexId;
	}

	public int getBackupNo() {
		return backupNo;
	}

	public void setBackupNo(int backupNo) {
		this.backupNo = backupNo;
	}

}
