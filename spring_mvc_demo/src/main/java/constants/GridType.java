package constants;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum GridType {

	UNDEFINED(-1), GRIDVIEW_SMALL(0), GRIDVIEW_MEDIUM(1), GRIDVIEW_LARGE(2);

	private Integer gridNo;

	private GridType(Integer gridNo) {
		this.gridNo = gridNo;
	}

	@JsonCreator
	public static GridType parse(Integer gridNo) {
		for (GridType gridType : GridType.values()) {
			if (gridType.getGridNo() == gridNo) {
				return gridType;
			}
		}
		return null;
	}

	@JsonValue
	public Integer getGridNo() {
		return gridNo;
	}

	public void setGridNo(Integer gridNo) {
		this.gridNo = gridNo;
	}

}
