package model.response;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import model.ActiveHistory;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetActivityHistoryResponse implements Serializable {

	private static final long serialVersionUID = 1L;
	@JsonProperty("last_edit_time")
	private String lastEditTime = "";
	@JsonProperty("last_periods")
	private int lastPeriods = 0;
	@JsonProperty("activities")
	private LinkedHashMap<String, Map<String, List<ActiveHistory>>>  activeHistories;
	
	


	public int getLastPeriods() {
		return lastPeriods;
	}
	public void setLastPeriods(int lastPeriods) {
		this.lastPeriods = lastPeriods;
	}
	public LinkedHashMap<String, Map<String, List<ActiveHistory>>> getActiveHistories() {
		return activeHistories;
	}
	public void setActiveHistories(
			LinkedHashMap<String, Map<String, List<ActiveHistory>>> activeHistories) {
		this.activeHistories = activeHistories;
	}
	public String getLastEditTime() {
		return lastEditTime;
	}
	public void setLastEditTime(String lastEditTime) {
		this.lastEditTime = lastEditTime;
	}


}
