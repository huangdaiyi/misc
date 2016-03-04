package model;

import constants.GridType;
import constants.SortType;
import constants.ViewType;

public class PageProfileProperty {

	private SortType sortByType;
	private ViewType viewByType;
	private GridType gridByType;

	public PageProfileProperty() {
		super();
	}

	public void resetSortByType(Iterable<ProfileProperty> properties, SortType forceSortByType) {
		if (forceSortByType != null) {
			sortByType = forceSortByType;
			return;
		}
		if (properties != null) {
			for (ProfileProperty item : properties) {
				if (item.getPropertyName().equals("sortby_type") && item.getDeviceUid().isEmpty()) {
					sortByType = item.toSortType();
					return;
				}
			}
		}
		sortByType = SortType.CREATED_AT;
	}

	public void resetViewByType(Iterable<ProfileProperty> properties, String viewerDeviceUniqueId) {
		if (properties != null) {
			for (ProfileProperty item : properties) {
				if (item.getPropertyName().equals("viewby_type") && item.getDeviceUid().equals(viewerDeviceUniqueId)) {
					viewByType = item.toViewType();
					return;
				}
			}
		}
		viewByType = ViewType.UNDEFINED;
	}

	public void resetGridByType(Iterable<ProfileProperty> properties, String viewerDeviceUniqueId) {
		if (properties != null) {
			for (ProfileProperty item : properties) {
				if (item.getPropertyName().equals("gridby_type") && item.getDeviceUid().equals(viewerDeviceUniqueId)) {
					gridByType = item.toGridType();
					return;
				}
			}
		}
		gridByType = GridType.UNDEFINED;
	}

	public SortType getSortByType() {
		return sortByType;
	}

	public void setSortByType(SortType sortByType) {
		this.sortByType = sortByType;
	}

	public ViewType getViewByType() {
		return viewByType;
	}

	public void setViewByType(ViewType viewByType) {
		this.viewByType = viewByType;
	}

	public GridType getGridByType() {
		return gridByType;
	}

	public void setGridByType(GridType gridByType) {
		this.gridByType = gridByType;
	}

}
