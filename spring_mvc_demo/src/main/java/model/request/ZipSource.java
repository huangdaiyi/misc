package model.request;

import model.PathRequestBase;
import model.ZipSourcePure;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ZipSource extends PathRequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("is_folder")
	private boolean folder;

	public ZipSource() {
		super();
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}
	
	public ZipSourcePure toZipSourcePure() {
		if(getOwnerId() == null) {
			setOwnerId("");
		}
		if(getSharedRootId() == null) {
			setSharedRootId("");
		}
		return new ZipSourcePure(getPath(), getSourcePath(), getOwnerId(), getSharedRootId(), folder);
	}

}
