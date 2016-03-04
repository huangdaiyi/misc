package model.request;

import model.PathRequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import constants.ProcessAction;
import constants.ProcessingStatus;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateFileProcessingStatusRequest extends PathRequestBase {

	private static final long serialVersionUID = 1;

	@JsonProperty("processing_status")
	private ProcessingStatus processingStatus = ProcessingStatus.PROCESSING;
	@JsonProperty("percentage")
	private float percentage = 0;
	@JsonProperty("item_in_progress")
	private String itemInProgress = "";
	@JsonProperty("process_action")
	private ProcessAction processAction = ProcessAction.NONE;
	@JsonProperty("is_folder")
	private boolean folder = false;

	public UpdateFileProcessingStatusRequest() {
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

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

}
