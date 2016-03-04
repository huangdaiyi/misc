package model.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import model.GlobalIcon;
import model.GlobalIconGroup;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetGlobalIconResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private List<GlobalIcon> files = new ArrayList<GlobalIcon>();
	private List<GlobalIconGroup> folders = new ArrayList<GlobalIconGroup>();
	
	public List<GlobalIcon> getFiles() {
		return files;
	}
	public void setFiles(List<GlobalIcon> files) {
		this.files = files;
	}
	public List<GlobalIconGroup> getFolders() {
		return folders;
	}
	public void setFolders(List<GlobalIconGroup> folders) {
		this.folders = folders;
	}
	
	
}
