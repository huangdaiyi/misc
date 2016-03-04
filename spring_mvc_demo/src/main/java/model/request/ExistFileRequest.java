package model.request;

import java.util.List;

import model.ExistFile;
import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;


@JsonIgnoreProperties(ignoreUnknown = true)
public class ExistFileRequest extends RequestBase {
	private static final long serialVersionUID = 1L;

	private List<ExistFile> items;
	@JsonProperty("include_folder")
	private boolean includeFolder = true;

	public ExistFileRequest() {
		super();
	}

	public List<ExistFile> getItems() {
		return items;
	}

	public void setItems(List<ExistFile> items) {
		this.items = items;
	}

	public boolean isIncludeFolder() {
		return includeFolder;
	}

	public void setIncludeFolder(boolean includeFolder) {
		this.includeFolder = includeFolder;
	}

}
