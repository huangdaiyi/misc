package constants;

import exception.MetadataException;

public enum HttpStatus {

	/*
	 * 2xx
	 */
	OK       (200, ""),
	CREATED  (201, ""),
	ACCEPTED (202, ""),

	/*
	 * 4xx
	 */
	ERROR_IN_PARAMETERS                    (400, "020001"),
	// !! 401 for invalid token only!!
	TOKEN_IS_INVALID                       (401, "020002"),
	METHOD_NOT_ALLOWED                     (405, "020003"),
	CANT_CREATE_FILE_IN_THIS_FOLDER        (403, "020004"),
	FILE_NOT_EXIST                         (404, "020005"),
	CANT_DELETE_THIS_FILE                  (403, "020006"),
	MOVE_FILE_DESTINATION_FOLDER_NOT_EXIST (404, "020007"),
	COPY_FILE_DESTINATION_FOLDER_NOT_EXIST (404, "020008"),
	DEVICE_NOT_EXIST                       (404, "020009"),
	DIR_NOT_EXIST                          (404, "020010"),
	CANT_CREATE_FOLDER_IN_THIS_FOLDER      (403, "020011"),
	CANT_NOTE_FOLDER                       (403, "020012"),
	CANT_COPY_FOLDER_FROM                  (403, "020013"),
	CANT_COPY_FOLDER_TO_OVERRIDE_FALSE     (403, "020014"),
	CANT_MOVE_COPY_FOLDER_EXTRACT          (403, "020015"),
	CANT_COPY_FOLDER_FROM_EXTRACT          (403, "020016"),
	FILE_EXIST                             (403, "020017"),
	NAME_IS_EMPTY                          (403, "020018"),
	TARGET_PATH_IS_FOLDER                  (403, "020019"),
	FOLDER_TYPE_IS_COMMON                  (403, "020020"),
	PARAM_NOT_EXIST                        (404, "020021"),
	CANT_LINK_FOLDER                       (403, "020022"),
	CANT_UNLINK_FOLDER                     (403, "020023"),
	CANT_ENCRYPT                           (403, "020024"),
	CANT_DECRYPT                           (403, "020025"),
	CANT_CREATE_COLLABORATE_MEMBER         (403, "020026"),
	PASSWORD_INCORRECT                     (412, "020027"),
	GATEWAY_FAILED                         (403, "020028"),
	CANT_ZIP_UNZIP                         (403, "020029"),
	ZIP_SOURCE_NOT_EXIST                   (404, "020031"),
	ICON_EXIST                             (403, "020032"),
	SHARE_LINK_NOT_EXIST                   (404, "020033"),
	CANT_ACCESS_COLLABORATE                (403, "020034"),
	COLLABORATE_MEMBER_NOT_EXIST           (404, "020035"),
	COLLABORATE_MEMBER_REGISTERED          (403, "020036"),
	BACKUP_COUNT_IS_NEGATIVE               (403, "020037"),
	CANT_MOVE_FILE                         (403, "020038"),
	FOLDER_NOT_EXIST                       (404, "020039"),
	UNDER_ENCRYPTED_FOLDER                 (403, "020040"),
	ENCRYPTED                              (403, "020041"),
	VIOLATE_ENCRYPTION_RULE                (403, "020042"),
	VIOLATE_ZIP_RULE                       (403, "020042"),
	CANT_CREATE_COLLABORATE                (403, "020043"),
	INVALID_SYNC_RELATION_ACTION           (400, "020044"),
	SOURCE_FOLDER_NOT_FOUND                (404, "020045"),
	TARGET_FOLDER_NOT_FOUND                (404, "020046"),
	SOURCE_IS_NOT_A_FOLDER                 (403, "020047"),
	TARGET_IS_NOT_A_FOLDER                 (403, "020048"),
	SOURCE_FOLDER_ALREADY_SYNC             (403, "020049"),
	TARGET_FOLDER_ALREADY_SYNC             (403, "020050"),
	UPPER_LEVEL_OF_SOURCE_ALREADY_SYNC     (403, "020051"),
	LOWER_LEVEL_OF_SOURCE_ALREADY_SYNC     (403, "020052"),
	UPPER_LEVEL_OF_TARGET_ALREADY_SYNC     (403, "020053"),
	LOWER_LEVEL_OF_TARGET_ALREADY_SYNC     (403, "020054"),
	CYCLE_SYNC                             (403, "020055"),
	RELATION_NOT_FOUND                     (404, "020056"),
	RELATION_EXIST                         (403, "020057"),
	NOT_ALLOW_RELATION_IN_SAME_PLACE       (403, "020058"),
	RELATION_ACTION_NOT_ALLOW              (403, "020059"),
	FOLDER_IS_NOT_A_SYNC_ROOT              (403, "020060"),
	TEXT_VIEW_SETTING_NOT_EXIST            (404, "020061"),
	FILE_NOT_ALLOW_ACCESS                  (403, "020062"),
	UNDER_PROCESSING                       (409, "020063"),
	STATUS_NOT_EXIST                       (404, "020065"),
	FOLDER_EXIST                           (403, "020066"),
	PROCESS_CANCELED                       (403, "020067"),
	CANT_AFFECT_ON_SHARED_FOLDER           (403, "020068"),
	STRING_IS_TOO_LONG                     (403, "020069"),
	VALUE_CANT_BE_NULL                     (403, "020070"),
	PROCESS_IS_LOCKED                      (403, "020071"),
	PROCESS_NOT_EXIST                      (403, "020072"),
	CANT_COPY_TO_SYNC_FOLDER               (403, "020073"),
	CANT_COPY_TO_OTHER_SYNC_FOLDER         (403, "020074"),
	CANT_MOVE_TO_SYNC_FOLDER               (403, "020075"),
	CANT_MOVE_TO_OTHER_SYNC_FOLDER         (403, "020076"),
	UNSUPPORT_COMMON_FOLDER_FILE_EXTENSION (403, "020077"),
	MAIL_NOT_EXIST                         (404, "020078"),
	CANT_MOVE_FOLDER_FROM                  (403, "020079"),
	CANT_DELETE_THIS_FOLDER                (403, "020080"),
	CANT_ACCESS_API                        (403, "020081"),
	INVITE_STATUS_NOT_FOUND                (404, "020082"),
	SMS_UNDELIVERED                        (400, "020083"),
	CANT_MOVE_FOLDER_TO                    (403, "020084"),

	/*
	 * 500
	 */
	INTERNAL_SERVER_ERROR (500, "");

	private final int code;
	private final String reason;

	private HttpStatus(int code, String reason) {
		this.code = code;
		this.reason = reason;
	}

	public int getCode() {
		return code;
	}

	public String getReason() {
		return reason;
	}
	
	public static HttpStatus fromCode(int code) {
		for (HttpStatus status : HttpStatus.values()) {
			if (code == status.getCode()) {
				return status;
			}
		}
		throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
	}

}
