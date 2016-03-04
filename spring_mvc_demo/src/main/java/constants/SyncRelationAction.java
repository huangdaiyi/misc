package constants;

public enum SyncRelationAction {
	COPY("copy"),
	MOVE("move"),
	SYNC("sync");
	
	private String action;
	
	private SyncRelationAction(String action) {
		this.action = action;
	}
	
	public static SyncRelationAction parse(String actionText) throws IllegalArgumentException {
		for (SyncRelationAction action : SyncRelationAction.values()) {
			if (action.toString().equals(actionText)) {
				return action;
			}
		}
		throw new IllegalArgumentException();
	}

	@Override
	public String toString() {
		return action;
	}
}
