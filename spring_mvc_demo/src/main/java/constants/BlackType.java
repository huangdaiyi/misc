package constants;

public enum BlackType {
	DEFAULT("default"),
	CONVERSATION("conversation");

	private String type;

	private BlackType(String type) {
		this.type = type;
	}

	@Override
	public String toString() {
		return this.type;
	}
}
