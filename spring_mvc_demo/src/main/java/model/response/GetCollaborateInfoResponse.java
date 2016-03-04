package model.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import model.CollaborateMember;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GetCollaborateInfoResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private String path;
	private String owner;
	@JsonProperty("owner_id")
	private String ownerId;
	private Boolean accepted;
	@JsonProperty("member_id")
	private Long memberId;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("files_count")
	private Integer filesCount;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("folders_count")
	private Integer foldersCount;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("subfilecount")
	private int subFileCount;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("subfoldercount")
	private int subFolderCount;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("size")
	private Long totalSize;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("icon_block_id")
	private String iconBlockId;
	@JsonProperty("icon_text")
	private String iconText = "";
	@JsonProperty("icon_text_color")
	private String iconTextColor = "";
	@JsonProperty("icon_text_style")
	private String iconTextStyle = "";
	@JsonProperty("note")
	private boolean hasNote;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("shared_root_id")
	private String metadataIndexId;
	private List<CollaborateMember> members;
	@JsonProperty("has_unread_file")
	private boolean hasUnreadFile;

	public GetCollaborateInfoResponse() {
		super();
	}

	public void setupMembers(List<CollaborateMember> members, CollaborateMember currentMember, boolean isDetail) {
		List<CollaborateMember> newMembers = new ArrayList<CollaborateMember>();
		for (CollaborateMember collaborateMember : members) {
			if (collaborateMember.getAccepted() != null && collaborateMember.getAccepted() == false) {
				continue;
			}
			if (!isDetail) {
				collaborateMember.setPhotoBlockId(null);
				collaborateMember.setPhotoSize(null);
			}
			collaborateMember.setMetadataIndexId(null);
			newMembers.add(collaborateMember);
		}
		this.members = newMembers;

		if (currentMember != null) {
			this.accepted = currentMember.getAccepted();
			this.memberId = currentMember.getMemberId();
		}
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getOwner() {
		return owner;
	}

	public void setOwner(String owner) {
		this.owner = owner;
	}

	public String getOwnerId() {
		return ownerId;
	}

	public void setOwnerId(String ownerId) {
		this.ownerId = ownerId;
	}

	public Boolean getAccepted() {
		return accepted;
	}

	public void setAccepted(Boolean accepted) {
		this.accepted = accepted;
	}

	public Integer getFilesCount() {
		return filesCount;
	}

	public void setFilesCount(Integer filesCount) {
		this.filesCount = filesCount;
	}

	public Integer getFoldersCount() {
		return foldersCount;
	}

	public void setFoldersCount(Integer foldersCount) {
		this.foldersCount = foldersCount;
	}

	public Long getMemberId() {
		return memberId;
	}

	public void setMemberId(Long memberId) {
		this.memberId = memberId;
	}

	public String getIconBlockId() {
		return iconBlockId;
	}

	public void setIconBlockId(String iconBlockId) {
		this.iconBlockId = iconBlockId;
	}

	public List<CollaborateMember> getMembers() {
		return members;
	}

	public void setMembers(List<CollaborateMember> members) {
		this.members = members;
	}

	public boolean isHasNote() {
		return hasNote;
	}

	public void setHasNote(boolean hasNote) {
		this.hasNote = hasNote;
	}

	public String getIconText() {
		return iconText;
	}

	public void setIconText(String iconText) {
		this.iconText = iconText;
	}

	public String getIconTextColor() {

		return iconTextColor;
	}

	public void setIconTextColor(String iconTextColor) {
		this.iconTextColor = iconTextColor;
	}

	public String getIconTextStyle() {
		return iconTextStyle;
	}

	public void setIconTextStyle(String iconTextStyle) {
		this.iconTextStyle = iconTextStyle;
	}

	public String getMetadataIndexId() {
		return metadataIndexId;
	}

	public void setMetadataIndexId(String metadataIndexId) {
		this.metadataIndexId = metadataIndexId;
	}

	public Long getTotalSize() {
		return totalSize;
	}

	public void setTotalSize(Long totalSize) {
		this.totalSize = totalSize;
	}

	public int getSubFileCount() {
		return subFileCount;
	}

	public void setSubFileCount(int subFileCount) {
		this.subFileCount = subFileCount;
	}

	public int getSubFolderCount() {
		return subFolderCount;
	}

	public void setSubFolderCount(int subFolderCount) {
		this.subFolderCount = subFolderCount;
	}

	public boolean isHasUnreadFile() {
		return hasUnreadFile;
	}

	public void setHasUnreadFile(boolean hasUnreadFile) {
		this.hasUnreadFile = hasUnreadFile;
	}

}