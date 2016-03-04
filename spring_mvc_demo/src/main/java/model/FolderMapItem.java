package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class FolderMapItem implements Serializable {

	private static final long serialVersionUID = 1L;

	private String targetId;
	private boolean duplicate;
	private String currentPId;
	
	
	public FolderMapItem(String targetId) {
		this(targetId, false);
	}
	
	public FolderMapItem(String targetId, String currentPid) {
		this(targetId,currentPid, false);
	}

	public FolderMapItem(String targetId,  boolean duplicate) {
		this(targetId, targetId, duplicate);
	}

	public FolderMapItem(String targetId,  String currentPid, boolean duplicate) {
		this.targetId = targetId;
		this.duplicate = duplicate;
		this.currentPId = currentPid;
	}


	public String getTargetId() {
		return targetId;
	}

	public void setTargetId(String targetId) {
		this.targetId = targetId;
	}


	public boolean isDuplicate() {
		return duplicate;
	}

	public void setDuplicate(boolean duplicate) {
		this.duplicate = duplicate;
	}

	public FolderMapItem() {
		super();
	}

	public String getCurrentPId() {
		return currentPId;
	}

	public void setCurrentPId(String currentPId) {
		this.currentPId = currentPId;
	}

}
