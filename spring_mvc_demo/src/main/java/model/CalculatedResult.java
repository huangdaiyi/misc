package model;

import java.util.Hashtable;
import java.util.List;

public class CalculatedResult {

	private List<List<Metadata>> resultList;
	private Hashtable<String, Metadata> resultTable;
	private List<Metadata> forUnbackUp;
	private List<MetadataAttr> needInsertCount;

	public List<MetadataAttr> getNeedInsertCount() {
		return needInsertCount;
	}

	public void setNeedInsertCount(List<MetadataAttr> needInsertCount) {
		this.needInsertCount = needInsertCount;
	}

	public List<Metadata> getForUnbackUp() {
		return forUnbackUp;
	}

	public void setForUnbackUp(List<Metadata> forUnbackUp) {
		this.forUnbackUp = forUnbackUp;
	}

	public List<List<Metadata>> getResultList() {
		return resultList;
	}

	public void setResultList(List<List<Metadata>> resultList) {
		this.resultList = resultList;
	}

	public Hashtable<String, Metadata> getResultTable() {
		return resultTable;
	}

	public void setResultTable(Hashtable<String, Metadata> resultTable) {
		this.resultTable = resultTable;
	}

}
