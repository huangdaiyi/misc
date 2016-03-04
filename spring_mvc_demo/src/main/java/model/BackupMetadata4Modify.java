package model;

public class BackupMetadata4Modify {

	private String id;
	private String originName;
	private int backupNo;

	public BackupMetadata4Modify(String id, String originName, int backupNo) {
		super();
		this.id = id;
		this.originName = originName;
		this.backupNo = backupNo;
	}

	public BackupMetadata4Modify() {
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return  this.originName.toLowerCase(); //StringUtils.formatName(this.originName.toLowerCase(), this.backupNo, true);
	}

	public String getOriginName() {
		return  originName; //StringUtils.formatName(this.originName, this.backupNo, true);
	}

	public void setOriginName(String originName) {
		this.originName = originName;
	}

	public int getBackupNo() {
		return backupNo;
	}

	public void setBackupNo(int backupNo) {
		this.backupNo = backupNo;
	}

}
