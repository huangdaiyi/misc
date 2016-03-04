package model.request;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CreateFileOrFolderRequest extends UpdateFileRequest {
	
	private static final long serialVersionUID = 1L;
	
	@JsonProperty("is_folder")
	private boolean isFolder;

	public boolean isFolder() {
		return isFolder;
	}

	public void setFolder(boolean isFolder) {
		this.isFolder = isFolder;
	}
	
	public UpdateFolderRequest toUpdateFolderRequest() {
		UpdateFolderRequest request = new UpdateFolderRequest();
		request.fromPathRequestBase(this);
		request.setNote(getNote());
		request.setIconBlockId(getIconBlockId());
		request.setIconText(getIconText());
		request.setIconTextColor(getIconTextColor());
		request.setIconTextStyle(getIconTextStyle());
		return request;
	}
}
