package constants;

public enum ChangeReason {
	NONE(""),
	ADD_USER_CELLPHONE("add user cellphone"),
	ARRANGE_FOLDER("arrange folder"),
	ALREADY_READ("read file"),
	CANCEL_COLLABORATE("cancel collaborate"),
	CREATE_COLLABORATE("create collaborate"),
	CREATE_COLLABORATE_MEMBER("create collaborate member"),
	CHANGE_USER_ID("change user id"),
	CREATE_FILE("create file"),
	CREATE_FOLDER("create folder"),
	COPY_FILE("copy file"),
	COPY_FOLDER("copy folder"),
	DELETE_BACKUPFILE("delete backupfile"),
	DELETE_COLLABORATE("delete collaborate"),
	DELETE_COLLABORATE_MEMBER("delete collaborate member"),
	DELETE_FILE("delete file"),
	DELETE_FOLDER("delete folder"),
	DELETE_USER_CELLPHONE("delete user cellphone"),
	DEVICE_LOGOUT("device logout"),
	DEVICE_REGISTER("device register"),
	DEVICE_RENAME("device rename"),
	DEVICE_UNREGISTER("device unregister"),
	DEVICE_UPDATE("device update"),
	DECRYPT_FILE("decrypt file"),
	DECRYPT_FOLDER("decrypt folder"),
	ENCRYPT_FILE("encrypt file"),
	ENCRYPT_FOLDER("encrypt folder"),
	INVITE_COLLABORATE_MEMBER("invite collaborate member"),
	LINK_FOLDER("link folder"),
	MOVE_FILE("move file"),
	MOVE_FOLDER("move folder"),
	RENAME_FILE("rename file"),
	RENAME_FOLDER("rename folder"),
	RESET_COMMON_FOLDER("reset common folder"),
	RESET_USER("reset user"),
	SET_ICON("set icon"),
	SUSPEND_USER("copy file"),
	UNLINK_FOLDER("unlink folder"),
	UPDATE_COLLABORATE("update collaborate"),
	UPDATE_COLLABORATE_MEMBER("update collaborate member"),
	UPDATE_FILE("update file"),
	UPDATE_PROCESSING_STATUS("update processing status"),
	UPDATE_PROFILE_PROPERTY("update profile property"),
	UPDATE_FOLDER("update folder"),
	UPDATE_NOTE("update note"),
	UPDATE_IMAGE("update image"),
	UPDATE_MEMBER_PHOTO("update member photo"),
	REJECT_THE_INVITATION("reject  the invitation"),
	ACCEPT_THE_INVITATION("accept the invitation"),
	WAIT_FOR_INVITATION("wait for confirmation"),
	USER_UPDATE_SETTINGS("user update settings"),
	SET_PROFILE_PROPERTY("set profile property"),
	DELETE_PROFILE_PROPERTY("delete profile property"),
	BULK_COPY("bulk copy"),
	BULK_MOVE("bulk move"),
	BULK_DELETE("bulk delete"),
	ADD_SYNC_RELATION("add sync relation"),
	UPDATE_SYNC_RELATION("update sync relation"),
	DELETE_SYNC_RELATION("delete sync relation"),
	TRANSFORM_SYNC_TO_NORMAL("transform sync folder to normal folder"),
	TRANSFORM_NORMAL_TO_SYNC("transform normal folder to sync folder"),
	SET_ITEM_STYLE("set item style"),
	SET_SUB_ITEMS_STYLE("set sub items style");;
	
	private String changeReason;
	
	private ChangeReason(String changeReason){
		this.setChangeReason(changeReason);
		
	}
	
	public String getChangeReason() {
		return changeReason;
	}

	public void setChangeReason(String changeReason) {
		this.changeReason = changeReason;
	}
	
	@Override
	public String toString() {
		return changeReason;
	}

	

}
