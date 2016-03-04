package constants;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum ProcessingStatus {
	SUCCESS(0), PROCESSING(1), SUCCESS_FOR_READER(2), FAILED(3), CANCEL(4), WAITING(5), LOCKING(6);
	
	private int processingStatus;
	
	private ProcessingStatus(int processingStatus) {
		this.processingStatus = processingStatus;
	}

	@JsonCreator
	public static ProcessingStatus parse(int processingStatus) {
		for (ProcessingStatus ps : ProcessingStatus.values()) {
			if (ps.getProcessingStatus() == processingStatus) {
				return ps;
			}
		}
		return null;
	}
	
	@JsonValue
	public int getProcessingStatus() {
		return processingStatus;
	}

	public void setProcessingStatus(int processingStatus) {
		this.processingStatus = processingStatus;
	}
	
	public boolean isUnderProcessing() {
		if (processingStatus == 1 || processingStatus == 5 || processingStatus == 4) {
			return true;
		}
		return false;
	}
	
}
