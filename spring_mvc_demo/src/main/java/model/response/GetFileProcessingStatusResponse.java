package model.response;

import java.io.Serializable;

import model.FileProcessingStatus;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import constants.ProcessAction;
import constants.ProcessingStatus;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetFileProcessingStatusResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonProperty("processing_status")
	private ProcessingStatus processingStatus;
	@JsonProperty("percentage")
	private float percentage;
	@JsonProperty("item_in_progress")
	private String itemInProgress;
	@JsonProperty("process_action")
	private ProcessAction processAction;
	@JsonProperty("can_cancel")
	private boolean canCancel;
	
	public GetFileProcessingStatusResponse() {
		super();
	}

	public ProcessingStatus getProcessingStatus() {
		return processingStatus;
	}

	public void setProcessingStatus(ProcessingStatus processingStatus) {
		this.processingStatus = processingStatus;
	}

	public float getPercentage() {
		return percentage;
	}

	public void setPercentage(float percentage) {
		this.percentage = percentage;
	}

	public String getItemInProgress() {
		return itemInProgress;
	}

	public void setItemInProgress(String itemInProgress) {
		this.itemInProgress = itemInProgress;
	}

	public ProcessAction getProcessAction() {
		return processAction;
	}

	public void setProcessAction(ProcessAction processAction) {
		this.processAction = processAction;
	}
	
	public boolean isCanCancel() {
		return canCancel;
	}

	public void setCanCancel(boolean canCancel) {
		this.canCancel = canCancel;
	}

	public GetFileProcessingStatusResponse fromFileProcessingStatus(FileProcessingStatus fileProcessingStatus) {
		this.processingStatus = fileProcessingStatus.getProcessingStatus();
		this.percentage = fileProcessingStatus.getPercentage();
		this.itemInProgress = fileProcessingStatus.getItemInProgress();
		this.processAction = fileProcessingStatus.getProcessAction();
		return this;
	}
}
