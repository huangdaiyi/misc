package constants;

public enum SystemEvent {
	NONE(""),
	ADD_USER_CELLPHONE("add_user_cellphone"),
	ARRANGE_FOLDER("arrange_folder"),
	CANCEL_COLLABORATE("cancel_collaborate"),
	CHANGE_USER_ID("change_user_id"),
	CREATE_FILE("create_file"),
	CREATE_FOLDER("create_folder"),
	COPY_FILE("copy_file"),
	COPY_FOLDER("copy_folder"),
	DELETE_BACKUPFILE("delete_backupfile"),
	DELETE_COLLABORATE("delete_collaborate"),
	DELETE_COLLABORATE_MEMBER("delete_collaborate_member"),
	DELETE_FILE("delete_file"),
	DELETE_FOLDER("delete_folder"),
	DELETE_USER_CELLPHONE("delete_user_cellphone"),
	DEVICE_LOGOUT("device_logout"),
	DEVICE_REGISTER("device_register"),
	DEVICE_RENAME("device_rename"),
	DEVICE_UNREGISTER("device_unregister"),
	DEVICE_UPDATE("device_update"),
	DECRYPT_FILE("decrypt_file"),
	DECRYPT_FOLDER("decrypt_folder"),
	ENCRYPT_FILE("encrypt_file"),
	ENCRYPT_FOLDER("encrypt_folder"),
	INVITE_COLLABORATE_MEMBER("invite_collaborate_member"),
	LINK_FOLDER("link_folder"),
	MOVE_FILE("move_file"),
	MOVE_FOLDER("move_folder"),
	RENAME_FILE("rename_file"),
	RENAME_FOLDER("rename_folder"),
	RESET_COMMON("reset_common"),
	RESET_USER("reset_user"),
	SET_ICON("set_icon"),
	SUSPEND_USER("copy_file"),
	UNLINK_FOLDER("unlink_folder"),
	UPDATE_COLLABORATE("update_collaborate"),
	UPDATE_COLLABORATE_MEMBER("update_collaborate_member"),
	UPDATE_FILE("update_file"),
	UPDATE_FILE_PROCESSING_STATUS("update_file_processing_status"),
	UPDATE_FOLDER("update_folder"),
	UPDATE_NOTE("update_note"),
	UPDATE_IMAGE("update_image"),
	UPDATE_MEMBER_PHOTO("update_member_photo"),
	USER_UPDATE_SETTINGS("user_update_settings"),
	SET_PROFILE_PROPERTY("set_profile_property"),
	DELETE_PROFILE_PROPERTY("delete_profile_property"),
	BULK_COPY("bulk_copy"),
	BULK_MOVE("bulk_move"),
	BULK_DELETE("bulk_delete"),
	BULK_CREATE("bulk_create"),
	UPDATE_SYNC_RELATION("update_sync_relation"),
	DELETE_SYNC_RELATION("delete_sync_relation"),
	ADD_SYNC_RELATION("add_sync_relation"),
	TRANSFORM_SYNC_TO_NORMAL("transform_sync_to_normal"),
	TRANSFORM_NORMAL_TO_SYNC("transform_normal_to_sync"),
	UPDATE_USER_DISK_SPACE("update_user_disk_space"),
	SET_ITEM_STYLE("set item style"),
	SET_SUB_ITEMS_STYLE("set sub items style");
	
	private String systemEvents;
	
	private SystemEvent(String systemEvents){
		this.systemEvents = systemEvents;
	}
	
	public void setSystemEvents(String systemEvents){
		this.systemEvents = systemEvents;
	}
	public String getSystemEvents(){
		return this.systemEvents.toLowerCase();
	}

}
