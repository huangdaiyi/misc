package model;

public class BackupMetadata extends BaseMetadata {

	private static final long serialVersionUID = 1L;

	private String originalIndexId = "";
	private int backupNo = 0;

	public BackupMetadata() {
		super();
		this.setBackup(true);
	}

	public boolean isIndependent() {
		return originalIndexId == null || originalIndexId.isEmpty();
	}

	public String getOriginalIndexId() {
		return originalIndexId;
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
