package auth;

import java.util.List;


import model.CellPhone;
import model.CollaborateMember;
import model.Metadata;
import utils.CellphonesUtils;
import utils.StringUtils;
import constants.HttpStatus;
import constants.MetadataType;
import exception.MetadataException;

public class CollaborateAuth  {
	private boolean owner = false;
	private Metadata collaborateFolder;
	@SuppressWarnings("unused")
	private String ownerId;
	private String userId;
	private Boolean isAccecpt = true;
	@SuppressWarnings("unused")
	private CollaborateAuth() {
	}
	public CollaborateAuth(Metadata collaborateFolder,String userId){
		this(collaborateFolder,userId,"");
	}
	public CollaborateAuth(Metadata collaborateFolder,String userId,String ownerId){
		this.collaborateFolder = collaborateFolder;
		this.ownerId = ownerId;
		this.userId = userId;
		isIllegal();
	}
	/**
	 * @return true,if user is owner
	*/
	public boolean isOwner(){
		if (userId.equals(collaborateFolder.getOwnerId())) {
			this.owner = true;
		}
		return this.owner;
	}
	/**
	 * @param members:all members of the collaborate folder
	 * @param userCellPhones:user's cellphone number
	 * 
	 * @return true,if user is  member
	*/
	public boolean isMember(List<CollaborateMember> members,
			List<CellPhone> userCellPhones) {
		boolean isCheck = false;
		if (members.isEmpty()) {
			isCheck = false;
		} else {
			String userFullCellphones = CellphonesUtils
					.composeCellphones(userCellPhones);
			String memberFullCellphones = null;
			for (CollaborateMember member : members) {
				memberFullCellphones = CellphonesUtils.composeCellphones(member
						.getCellphones());
				if (CellphonesUtils.compareCellphones(memberFullCellphones,
						userFullCellphones)
						&& doCheckAccept(member.getAccepted())) {
					isCheck = true;
					break;
				}
			}
		}
		return isCheck;
	}
	/**
	 * 
	 * @throws if user not owber and member,or getSharedRootId = "" or type != share
	 */
	private void isIllegal(){
		if (this.collaborateFolder == null
				|| StringUtils.isNullOrEmpty(this.collaborateFolder.getSharedRootId())
				|| !MetadataType.SHARE.toString().equals(this.collaborateFolder.getType())) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}
	}
	/**
	 *  @return true,if member accepted,or not check 
	 * 
	 */
	private boolean doCheckAccept(Boolean isAccepted) {
		if (this.isAccecpt != null && this.isAccecpt == true) {
			return (isAccepted != null && isAccepted == true);
		}else if (this.isAccecpt == null) {
			return (isAccepted == null ||  isAccepted == true);
		}
		return true;
	}
	public void setAccept(Boolean accept){
		this.isAccecpt = accept;
	}
}
