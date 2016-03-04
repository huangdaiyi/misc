package constants;

public enum XmppDeviceStateType {

	ONLINE("on-line"), OFFLINE("off-line"), ALL("all");

	String deviceState;

	private XmppDeviceStateType(String chatState) {
		this.deviceState = chatState;
	}

	public String toString() {
		return this.deviceState;
	}

}