package constants;

public enum SyncType {
	SYNC("sync"),
	UNSYNC("unsync");
	
	private String type;
	
	private SyncType(String type){
		this.type = type;
	}
	
	public static SyncType parse(String typeText) throws IllegalArgumentException {
		for (SyncType type : SyncType.values()) {
			if (type.toString().equals(typeText)) {
				return type;
			}
		}
		throw new IllegalArgumentException();
	}

	@Override
	public String toString() {
		return type;
	}
}
