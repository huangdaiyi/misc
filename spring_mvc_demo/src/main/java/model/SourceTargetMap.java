package model;

public class SourceTargetMap {

	private String sourceId;
	private String targetId;

	public String getSourceId() {
		return sourceId;
	}

	public void setSourceId(String sourceId) {
		this.sourceId = sourceId;
	}

	public String getTargetId() {
		return targetId;
	}

	public void setTargetId(String targetId) {
		this.targetId = targetId;
	}

	public SourceTargetMap() {

	}

	public SourceTargetMap(String sourceId, String targetId) {
		super();
		this.sourceId = sourceId;
		this.targetId = targetId;
	}

}
