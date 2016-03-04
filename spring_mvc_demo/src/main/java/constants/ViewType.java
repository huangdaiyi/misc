package constants;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum ViewType {

//	UNDEFINED(-1), NAME_ONLY(0), NAME_WITH_DEVICE(1), NAME_DETAIL(2), WITH_NOTE(3), NAME_WITH_LAST_MODIFIED(4), GRIDVIEW_SMALL(5), 
//	GRIDVIEW_MEDIUM(6), GRIDVIEW_LARGE(7), SECTION_LOCATION(8), SECTION_DATE(9), SECTION_NAME(10), SECTION_ARTIST(11),
//	GRID_SECTION_LOACTION_SMALL(12), GRID_SECTION_LOACTION_MEDIUM(13), GRID_SECTION_LOACTION_LARGE(14),
//	GRID_SECTION_DATE_SMALL(15), GRID_SECTION_DATE_MEDIUM(16), GRID_SECTION_DATE_LARGE(17),;

	UNDEFINED(-1), NAME_ONLY(0), NAME_WITH_DEVICE(1), NAME_DETAIL(2), WITH_NOTE(3), NAME_WITH_LAST_MODIFIED(4), MUSIC_LOCATION(5), MUSIC_NAME(6), MUSIC_ARTIST(7), PHOTO_LOCATION(8), PHOTO_DATE(9);
	
	private Integer viewNo;

	private ViewType(Integer viewNo) {
		this.viewNo = viewNo;
	}

	@JsonCreator
	public static ViewType parse(Integer viewNo) {
		for (ViewType viewType : ViewType.values()) {
			if (viewType.getViewNo() == viewNo) {
				return viewType;
			}
		}
		return null;
	}

	@JsonValue
	public Integer getViewNo() {
		return viewNo;
	}

	public void setViewNo(Integer viewNo) {
		this.viewNo = viewNo;
	}

}
