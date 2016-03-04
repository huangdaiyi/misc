package constants;

public enum CommonFolders {

	MY_DOCUMENTS("My Documents"),
	MY_DOCUMENTS_ZIP("My Documents/Zip"),
	MY_DOCUMENTS_EXCEL("My Documents/Excel"),
	MY_DOCUMENTS_PDF("My Documents/PDF"),
	MY_DOCUMENTS_PPT("My Documents/Power Point"),
	MY_DOCUMENTS_TEXT("My Documents/Text"),
	MY_DOCUMENTS_WORD("My Documents/Word"),
	MY_MUSIC("My Music"),
	MY_PICTURES("My Pictures"),
	MY_VIDEO("My Video"),
	MY_SHARED_FOLDERS("My Shared Folders"),
	My_STORAGE_DATA("My Storage Data"),
	MY_DEVICE_SYNC_FOLDERS("My Device Sync Folders"),
	MY_BACKUP_DATA("My Backup Data"),
	MY_SYNC_APP_BACKUP_DATA("My Sync App Backup Data"),
	MY_COMMUNICATION_DATA("My Communication Data"),
	MY_COMMUNICATION_DATA_CHAT("My Communication Data/Chat"),
	MY_COMMUNICATION_DATA_CONTACT("My Communication Data/Contact");

	private String originPath;
	private String path;

	private CommonFolders(String commonFolderOriginPath) {
		this.originPath = commonFolderOriginPath;
		this.path = commonFolderOriginPath.toLowerCase();
	}

	public String getOriginPath() {
		return originPath;
	}

	public void setOriginPath(String originPath) {
		this.originPath = originPath;
	}

	public static CommonFolders parse(String path,boolean isUnderBackup) {
		for (CommonFolders commonFolder : CommonFolders.values()) {
			if (isUnderBackup){
				if (MY_BACKUP_DATA.toString().concat("/").concat(commonFolder.toString()).equalsIgnoreCase(path)) {
					return commonFolder;
				}
			}else{
				if (commonFolder.toString().equalsIgnoreCase(path)) {
					return commonFolder;
				}
			}
		}
		return null;
	}

	@Override
	public String toString() {
		return path;
	}

	public enum DocumentFolders {
		EXCEL("Excel", "excel"),
		PDF("PDF", "pdf"),
		POWERPOINT("Power Point", "power point"),
		TEXT("Text", "text"),
		WORD("Word", "word"),
		ZIP("Zip", "zip");

		private String name;
		private String originName;

		private DocumentFolders(String originName, String name) {
			this.originName = originName;
			this.name = name;
		}

		public String getOriginName() {
			return originName;
		}

		public void setOriginName(String originName) {
			this.originName = originName;
		}

		@Override
		public String toString() {
			return this.name;
		}

	}
}
