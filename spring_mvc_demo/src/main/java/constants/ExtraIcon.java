package constants;

public enum ExtraIcon {
	TEXT_ICON("txt","1b77f878ccfcbfcf68ae36ab62f92006514d6c6677ea6c404204480054fe700f"),
	COMPRESS_ZIP_ICON("zip","234f39277578a6da06a8de63a3329a4dca6934435a330a47096a31d27f89c6f6"),
	COMPRESS_7Z_ICON("7z","234f39277578a6da06a8de63a3329a4dca6934435a330a47096a31d27f89c6f6"),
	COMPRESS_RAR_ICON("rar","234f39277578a6da06a8de63a3329a4dca6934435a330a47096a31d27f89c6f6"),
	MUSIC_MP3_ICON("mp3","18fb9105eaeb672728189d148233127c6392de9d08a44ea96f194fb00a8464de"),
	MUSIC_WAV_ICON("wav","18fb9105eaeb672728189d148233127c6392de9d08a44ea96f194fb00a8464de"),
	MUSIC_WMA_ICON("wma","18fb9105eaeb672728189d148233127c6392de9d08a44ea96f194fb00a8464de"),
	MUSIC_3GA_ICON("3ga","18fb9105eaeb672728189d148233127c6392de9d08a44ea96f194fb00a8464de");

	private String extension;
	private String blockId;
	
	private ExtraIcon(String extension,String blockId){
		this.extension = extension;
		this.blockId = blockId;
	}
	
	@Override
	public String toString() {
		return extension + "_" + blockId;
	}

	public String getExtension() {
		return extension;
	}

	public String getBlockId() {
		return blockId;
	}
	
}
