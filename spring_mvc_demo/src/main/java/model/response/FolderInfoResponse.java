package model.response;

import java.io.Serializable;
import java.util.List;

import model.Metadata;
import utils.MetadataUtils;

public class FolderInfoResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private int totalPage;
	private String sortBy;
	private List<Metadata> metadatas;

	public FolderInfoResponse() {
		super();
	}

	public List<Metadata> getFolders() {
		return metadatas == null ? null : MetadataUtils.findAllFolders(metadatas);
	}

	public List<Metadata> getFiles() {
		return metadatas == null ? null : MetadataUtils.findAllFolders(metadatas);
	}

	public int getTotalPage() {
		return totalPage;
	}

	public void setTotalPage(int totalPage) {
		this.totalPage = totalPage;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public List<Metadata> getMetadatas() {
		return metadatas;
	}

	public void setMetadatas(List<Metadata> metadatas) {
		this.metadatas = metadatas;
	}

}
