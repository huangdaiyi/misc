package constants;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum ProcessAction {
	ZIP("zip"), UNZIP("unzip"), ENCRYPT("encrypt"), DECRYPT("decrypt"), NONE("");
	
	private String processAction;
	
	private ProcessAction(String processAction) {
		this.processAction = processAction;
	}
	
	@JsonCreator
	public static ProcessAction parse(String processAction) {
		for (ProcessAction pa : ProcessAction.values()) {
			if (pa.getProcessAction().equals(processAction)) {
				return pa;
			}
		}
		return ProcessAction.NONE;
	}
	
	@JsonValue
	public String getProcessAction() {
		return processAction;
	}

	public void setProcessAction(String processAction) {
		this.processAction = processAction;
	}
}
