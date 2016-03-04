package constants;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum SortType {

	CREATED_AT(-1), NAME(0), SIZE(1), TYPE(2), MODIFIED_AT(3), USER_ARRANGE(4), DEVICE_NAME(5), SOURCE_PATH(6), EXTENSION(7), PATH(8);

	private int sortNo;

	private SortType(int sortNo) {
		this.sortNo = sortNo;
	}

	@JsonCreator
	public static SortType parse(int sortNo) {
		for (SortType sortType : SortType.values()) {
			if (sortType.getSortNo() == sortNo) {
				return sortType;
			}
		}
		return SortType.CREATED_AT;
	}

	@JsonValue
	public int getSortNo() {
		return sortNo;
	}

	public void setSortNo(int sortNo) {
		this.sortNo = sortNo;
	}

}
