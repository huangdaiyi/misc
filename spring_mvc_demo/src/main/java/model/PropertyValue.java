package model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import constants.SortType;
import constants.ViewType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PropertyValue implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private SortType sortByValue;
	private ViewType viewByValue;
	

	public PropertyValue() {
		super();
	}


	public SortType getSortByValue() {
		return sortByValue;
	}


	public void setSortByValue(SortType sortByValue) {
		this.sortByValue = sortByValue;
	}


	public ViewType getViewByValue() {
		return viewByValue;
	}


	public void setViewByValue(ViewType viewByValue) {
		this.viewByValue = viewByValue;
	}

	
}
