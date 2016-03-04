package erlang_to_java;

public class MetadataIndex {

	private String id;
	private String foldersCount;
	private String filesCount;
	private String size;
	private String params;

	public MetadataIndex() {
		super();
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getFoldersCount() {
		return foldersCount;
	}

	public void setFoldersCount(String foldersCount) {
		this.foldersCount = foldersCount;
	}

	public String getFilesCount() {
		return filesCount;
	}

	public void setFilesCount(String filesCount) {
		this.filesCount = filesCount;
	}

	public String getSize() {
		return size;
	}

	public void setSize(String size) {
		this.size = size;
	}

	public String getParams() {
		return params;
	}

	public void setParams(String params) {
		this.params = params;
	}

}
