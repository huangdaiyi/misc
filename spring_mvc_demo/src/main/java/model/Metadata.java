package model;

public class Metadata extends BaseMetadata {

	private static final long serialVersionUID = 1L;

	private int backupCount = -1;

	public Metadata() {
		super();
	}

	public int getBackupCount() {
		return backupCount;
	}

	public void setBackupCount(int backupCount) {
		this.backupCount = backupCount;
	}

}
