#!/bin/bash
##
## migrate erlang to java
##

DBHost="$1"
DBPort='3306'
DBUser="$2"
DBName="$3"
TargetDirPath="$(dirname "$0" | xargs readlink -f)"

if [ -z "$DBHost" ] || [ -z "$DBUser" ] || [ -z "$DBName" ]; then
	echo "usage: ./migrate_erlang_to_java.sh {DBHost} {DBUser} {DBName}"
	exit
fi

## password
echo "please backup your database and continue."
read -s -p "DBPassword: " DBPassword
echo ""

##
## private variables
##
SqlclientQueryCmd="mysql -h $DBHost -P $DBPort -u $DBUser --password=$DBPassword -D $DBName -B -N -s --max_allowed_packet=2G"

## create temp dir
Now=$(date +"%Y%m%d_%H%M%S_%N")
WorkTempDir="$TargetDirPath"/temp_"$Now"
mkdir -p "$WorkTempDir"

##
## private functions
##
fetch_metadata_index_id_by_parent_id() {
	MetadataIndexId="$1"
	IgnoreParentMetadataIndexIds="$2"
	echo "'$MetadataIndexId'"
	
	AdditionalCondition=""
	if [ ! -z "$IgnoreParentMetadataIndexIds" ]; then
		AdditionalCondition="AND mi.parent_id NOT IN ($IgnoreParentMetadataIndexIds)"
	fi
	$SqlclientQueryCmd -e "SELECT mi.id FROM metadata_index mi WHERE mi.parent_id = '$MetadataIndexId' $AdditionalCondition" | while read -r ChildMetadataIndexId; do
		fetch_metadata_index_id_by_parent_id "$ChildMetadataIndexId" "$IgnoreParentMetadataIndexIds"
	done
}

## ----------------------------------------------------------------------------------------------------
## collaborate
## ----------------------------------------------------------------------------------------------------
migrate_collaborate() {
	echo 'migrating collaborate...'

	$SqlclientQueryCmd -e "ALTER TABLE \`collaborate\` DROP INDEX \`metadata_id_idx\`, \
		DROP COLUMN \`metadata_id\`, \
		CHANGE COLUMN \`metadata_index_id\` \`metadata_index_id\` CHAR(36) NOT NULL"
	if [ -f "$WorkTempDir"/metadata_index.mapping ]; then
		$SqlclientQueryCmd -e "SELECT metadata_index_id FROM collaborate WHERE char_length(group_id) != 36" | while read -r MetadataIndexId; do
			NewMetadataIndexId="$(grep ^$MetadataIndexId, "$WorkTempDir"/metadata_index.mapping | awk -F, '{print $2}')"
			$SqlclientQueryCmd -e "UPDATE collaborate SET metadata_index_id = '$NewMetadataIndexId' WHERE metadata_index_id = '$MetadataIndexId'"
		done
		$SqlclientQueryCmd -e "DELETE FROM collaborate WHERE char_length(group_id) != 36"
	fi
}

## ----------------------------------------------------------------------------------------------------
## collaborate_member
## ----------------------------------------------------------------------------------------------------
migrate_collaborate_member() {
	echo 'migrating collaborate_member...'
	
	$SqlclientQueryCmd -e "ALTER TABLE \`collaborate_member\` DROP COLUMN \`photo_blocks\`, \
		CHANGE COLUMN \`member_id\` \`member_id\` CHAR(36) NOT NULL, \
		CHANGE COLUMN \`metadata_index_id\` \`metadata_index_id\` CHAR(36) NOT NULL"
	if [ -f "$WorkTempDir"/metadata_index.mapping ]; then
		$SqlclientQueryCmd -e "SELECT member_id, metadata_index_id FROM collaborate_member WHERE char_length(member_id) != 36" | while read -r MemberId MetadataIndexId; do
			NewMemberId="$(uuidgen)"
			NewMetadataIndexId="$(grep ^$MetadataIndexId, "$WorkTempDir"/metadata_index.mapping | awk -F, '{print $2}')"
			$SqlclientQueryCmd -e "UPDATE collaborate_member SET member_id = '$NewMemberId', metadata_index_id = '$NewMetadataIndexId' WHERE member_id = '$MemberId'"
			echo "$MemberId","$NewMemberId"
		done > "$WorkTempDir"/collaborate_member.mapping
		$SqlclientQueryCmd -e "DELETE FROM collaborate_member WHERE char_length(metadata_index_id) != 36"
	fi
}

## ----------------------------------------------------------------------------------------------------
## default_backup_setting
## ----------------------------------------------------------------------------------------------------
migrate_default_backup_setting() {
	echo 'migrating default_backup_setting...'
	
	$SqlclientQueryCmd -e "ALTER TABLE \`default_backup_setting\` CHANGE COLUMN \`group_id\` \`group_id\` CHAR(36) NOT NULL"
	if [ -f "$WorkTempDir"/default_backup_setting_group.mapping ]; then
		$SqlclientQueryCmd -e "SELECT group_id FROM default_backup_setting WHERE char_length(group_id) != 36" | while read -r GroupId; do
			NewGroupId="$(grep ^$GroupId, "$WorkTempDir"/default_backup_setting_group.mapping | awk -F, '{print $2}')"
			$SqlclientQueryCmd -e "UPDATE default_backup_setting SET group_id = '$NewGroupId' WHERE group_id = '$GroupId'"
		done
	fi
}

## ----------------------------------------------------------------------------------------------------
## default_backup_setting_group
## ----------------------------------------------------------------------------------------------------
migrate_default_backup_setting_group() {
	echo 'migrating default_backup_setting_group...'
	
	$SqlclientQueryCmd -e "ALTER TABLE \`default_backup_setting_group\` CHANGE COLUMN \`id\` \`id\` CHAR(36) NOT NULL"
	$SqlclientQueryCmd -e "SELECT id FROM default_backup_setting_group WHERE char_length(id) != 36" | while read -r GroupId; do
		NewGroupId=""
		case "$GroupId" in
			"10") NewGroupId='b29da3a8-094c-41ef-8ce3-ca633af49182' ;;
			"20") NewGroupId='8ddbf9e8-f8b6-4057-9fcb-6e759552b3cc' ;;
			"30") NewGroupId='88b85382-33ac-4d4c-bc08-bf8be94d608a' ;;
			"40") NewGroupId='46f3a793-4789-4adb-8893-2721678033d9' ;;
			"50") NewGroupId='b781c3fc-13ed-4a97-861b-b6df63712e67' ;;
			"60") NewGroupId='e94bea66-6083-445d-81ed-d1f0fd7d1af0' ;;
			"70") NewGroupId='fbafd61a-58e0-46ab-bf5f-790b1291c950' ;;
			"80") NewGroupId='95095aaf-072f-4a67-89e5-429ed61ebda3' ;;
			"90") NewGroupId='2d672d39-ad12-4f96-9cac-59d9edbb1cee' ;;
			*) NewGroupId="$(uuidgen)" ;;
			esac
		$SqlclientQueryCmd -e "UPDATE default_backup_setting_group SET id = '$NewGroupId' WHERE id = '$GroupId'"
		echo "$GroupId","$NewGroupId"
	done > "$WorkTempDir"/default_backup_setting_group.mapping
}

## ----------------------------------------------------------------------------------------------------
## device_info
## ----------------------------------------------------------------------------------------------------
migrate_device_info() {
	echo 'migrating device_info...'
	
	$SqlclientQueryCmd -e "ALTER TABLE \`device_info\` DROP COLUMN \`Status\`, \
		DROP COLUMN \`google_register_id\`, \
		DROP COLUMN \`Device_Name\`, \
		CHANGE COLUMN \`ID\` \`id\` INT(11) NOT NULL, \
		CHANGE COLUMN \`UserName\` \`user_id\` VARCHAR(100) NOT NULL, \
		CHANGE COLUMN \`Device_Id\` \`device_id\` VARCHAR(100) NULL, \
		CHANGE COLUMN \`Last_Edit_User\` \`last_edit_user\` VARCHAR(100) NOT NULL, \
		CHANGE COLUMN \`Last_Edit_Time\` \`last_edit_time\` DATETIME NOT NULL, \
		CHANGE COLUMN \`Unique_Id\` \`unique_id\` VARCHAR(100) NOT NULL, \
		COMMENT = ''"
}

## ----------------------------------------------------------------------------------------------------
## user_backup_setting
## ----------------------------------------------------------------------------------------------------
migrate_user_backup_setting() {
	echo 'migrating user_backup_setting...'
	
	$SqlclientQueryCmd -e "ALTER TABLE \`user_backup_setting\` CHANGE COLUMN \`group_id\` \`group_id\` CHAR(36) NOT NULL"
	if [ -f "$WorkTempDir"/default_backup_setting_group.mapping ]; then
		$SqlclientQueryCmd -e "SELECT group_id FROM user_backup_setting WHERE char_length(group_id) != 36" | while read -r GroupId; do
			NewGroupId="$(grep ^$GroupId, "$WorkTempDir"/default_backup_setting_group.mapping | awk -F, '{print $2}')"
			$SqlclientQueryCmd -e "UPDATE user_backup_setting SET group_id = '$NewGroupId' WHERE group_id = '$GroupId'"
		done
	fi
}

## ----------------------------------------------------------------------------------------------------
## global_folder_icon_group
## ----------------------------------------------------------------------------------------------------
migrate_global_folder_icon_group() {
	echo 'migrating global_folder_icon_group...'
	
	$SqlclientQueryCmd -e "ALTER TABLE \`global_folder_icon_group\` CHANGE COLUMN \`id\` \`id\` CHAR(36) NOT NULL"
	$SqlclientQueryCmd -e "SELECT id FROM global_folder_icon_group WHERE char_length(id) != 36" | while read -r GroupId; do
		NewGroupId="$(uuidgen)"
		$SqlclientQueryCmd -e "UPDATE global_folder_icon_group SET id = '$NewGroupId' WHERE id = '$GroupId'"
		echo "$GroupId","$NewGroupId"
	done > "$WorkTempDir"/global_folder_icon_group.mapping
}

## ----------------------------------------------------------------------------------------------------
## global_folder_icon
## ----------------------------------------------------------------------------------------------------
migrate_global_folder_icon() {
	echo 'migrating global_folder_icon...'
	
	$SqlclientQueryCmd -e "ALTER TABLE \`global_folder_icon\` CHANGE COLUMN \`block_Id\` \`block_id\` VARCHAR(70) NOT NULL, \
		CHANGE COLUMN \`default\` \`default\` BIT(1) NULL, \
		CHANGE COLUMN \`type\` \`type\` VARCHAR(20) NULL, \
		CHANGE COLUMN \`id\` \`id\` CHAR(36) NOT NULL, \
		CHANGE COLUMN \`group_id\` \`group_id\` CHAR(36) NOT NULL DEFAULT ''"
	$SqlclientQueryCmd -e "UPDATE global_folder_icon SET group_id = '' WHERE group_id IS NULL OR group_id = '0'"
	if [ -f "$WorkTempDir"/global_folder_icon_group.mapping ]; then
		$SqlclientQueryCmd -e "SELECT id, group_id FROM global_folder_icon WHERE char_length(id) != 36" | while read -r IconId GroupId; do
			NewIconId="$(uuidgen)"
			NewGroupId=""
			if [ ! -z "$GroupId" ]; then
				NewGroupId="$(grep ^$GroupId, "$WorkTempDir"/global_folder_icon_group.mapping | awk -F, '{print $2}')"
			fi
			$SqlclientQueryCmd -e "UPDATE global_folder_icon SET id = '$NewIconId', group_id = '$NewGroupId' WHERE id = '$IconId'"
			echo "$IconId","$NewIconId"
		done > "$WorkTempDir"/global_folder_icon.mapping
	fi
}

## ----------------------------------------------------------------------------------------------------
## metadata_index & backup_metadata_index
## ----------------------------------------------------------------------------------------------------
migrate_metadata_index_and_backup_metadata_index() {
	echo 'migrating metadata_index and backup_metadata_index...'
	
	$SqlclientQueryCmd -e "UPDATE \`metadata_index\` SET \`note\` = '' WHERE \`note\` IS NULL"
	$SqlclientQueryCmd -e "UPDATE \`metadata_index\` SET \`params\` = '' WHERE \`params\` IS NULL"
	$SqlclientQueryCmd -e "UPDATE \`metadata_index\` SET \`icon_block_id\` = '' WHERE \`icon_block_id\` IS NULL"
	$SqlclientQueryCmd -e "ALTER TABLE \`metadata_index\` DROP INDEX \`idx_metadata_index_parent_id_original_index_id_id\`, \
		DROP COLUMN \`encrypted_folder_key\`, \
		DROP COLUMN \`in_date\`, \
		DROP COLUMN \`last_edit_time\`, \
		CHANGE COLUMN \`id\` \`id\` CHAR(36) NOT NULL, \
		CHANGE COLUMN \`parent_id\` \`parent_id\` CHAR(36) NOT NULL DEFAULT '', \
		CHANGE COLUMN \`name\` \`name\` VARCHAR(200) NOT NULL DEFAULT '', \
		CHANGE COLUMN \`origin_name\` \`origin_name\` VARCHAR(200) NOT NULL DEFAULT '', \
		CHANGE COLUMN \`backup_count\` \`backup_count\` INT(11) NOT NULL DEFAULT '-1', \
		CHANGE COLUMN \`device_uid\` \`device_uid\` VARCHAR(100) NOT NULL DEFAULT '', \
		CHANGE COLUMN \`link_path\` \`link_path\` VARCHAR(5000) NOT NULL DEFAULT '', \
		CHANGE COLUMN \`note\` \`note\` VARCHAR(5000) NOT NULL DEFAULT '', \
		CHANGE COLUMN \`params\` \`params\` VARCHAR(5000) NOT NULL DEFAULT '', \
		CHANGE COLUMN \`icon_block_id\` \`icon_block_id\` VARCHAR(70) NOT NULL, \
		ADD COLUMN \`modified_at\` BIGINT(20) NOT NULL DEFAULT 0 AFTER \`origin_name\`, \
		ADD COLUMN \`modified_by\` VARCHAR(45) NOT NULL DEFAULT '' AFTER \`modified_at\`, \
		ADD COLUMN \`created_at\` BIGINT(20) NOT NULL DEFAULT 0 AFTER \`is_encrypted\`, \
		ADD COLUMN \`created_by\` VARCHAR(45) NOT NULL DEFAULT '' AFTER \`created_at\`, \
		ADD COLUMN \`block_id\` VARCHAR(96) NOT NULL DEFAULT '' AFTER \`created_by\`, \
		ADD COLUMN \`shared_root_id\` CHAR(36) NOT NULL DEFAULT '' AFTER \`block_id\`, \
		ADD COLUMN \`sync_root_id\` CHAR(36) NOT NULL DEFAULT '' AFTER \`shared_root_id\`, \
		ADD COLUMN \`type\` VARCHAR(20) NOT NULL DEFAULT 'normal' AFTER \`sync_root_id\`, \
		ADD COLUMN \`owner_id\` VARCHAR(45) NOT NULL DEFAULT '' AFTER \`type\`, \
		ADD COLUMN \`is_visible\` BIT(1) NOT NULL DEFAULT b'1' AFTER \`owner_id\`, \
		ADD INDEX \`idx_metadata_index_owner_id\` (\`owner_id\` ASC), \
		ADD INDEX \`idx_metadata_index_name\` (\`name\` ASC)"
	$SqlclientQueryCmd -e "UPDATE \`metadata_index\` SET \`parent_id\` = '' WHERE \`parent_id\` = '0'"
	
	echo 'merging metadata_index and metadata...'
	change_metadata_index_id_and_parent_id
	
	echo 'updating shared_root_id...'
	update_shared_root_id
	
	echo 'copy additional_attrs...'
	copy_additional_attrs
	
	echo 'moving my_backup_data...'
	move_my_backup_data
	
	echo 'drop unnecessary fields...'
	$SqlclientQueryCmd -e "ALTER TABLE \`metadata_index\` DROP COLUMN \`files_count\`, \
		DROP COLUMN \`folders_count\`, \
		DROP COLUMN \`backup_no\`, \
		DROP COLUMN \`metadata_id\`, \
		DROP COLUMN \`original_index_id\`"
}

change_metadata_index_id_and_parent_id() {
	$SqlclientQueryCmd -e "SELECT name FROM user_info WHERE status = 'active'" | while read -r UserId; do
		RootId=$($SqlclientQueryCmd -e "SELECT id FROM metadata_index WHERE parent_id = '' AND name = '$UserId' LIMIT 1")
		sub_change_metadata_index_id_and_parent_id "$RootId" ""
	done > "$WorkTempDir"/metadata_index.mapping
	
	$SqlclientQueryCmd -e "DELETE FROM metadata_index WHERE char_length(id) != 36"
}

sub_change_metadata_index_id_and_parent_id() {
	MetadataIndexId="$1"
	NewParentMetadataIndexId="$2"
	NewMetadataIndexId="$(uuidgen)"
	
	$SqlclientQueryCmd -e "UPDATE metadata_index SET id = '$NewMetadataIndexId', parent_id = '$NewParentMetadataIndexId' WHERE id = '$MetadataIndexId'"
	echo "$MetadataIndexId","$NewMetadataIndexId"
	
	$SqlclientQueryCmd -e "SELECT mi.id FROM metadata_index mi WHERE mi.parent_id = '$MetadataIndexId'" | while read -r ChildMetadataIndexId; do
		sub_change_metadata_index_id_and_parent_id "$ChildMetadataIndexId" "$NewMetadataIndexId"
	done
}

update_shared_root_id() {
	$SqlclientQueryCmd -e "SELECT name FROM user_info WHERE status = 'active'" | while read -r UserId; do
		RootId=$($SqlclientQueryCmd -e "SELECT id FROM metadata_index WHERE parent_id = '' AND name = '$UserId' LIMIT 1")
		MySharedFoldersId=$($SqlclientQueryCmd -e "SELECT id FROM metadata_index WHERE parent_id = '$RootId' AND name = 'my shared folders' LIMIT 1")
		
		$SqlclientQueryCmd -e "SELECT id FROM metadata_index WHERE parent_id = '$MySharedFoldersId'" | while read -r SharedFolderId; do
			SharedMetadataIndexIds=$(fetch_metadata_index_id_by_parent_id "$SharedFolderId" "" | tr '\n' ',' | sed 's/,$//')
			$SqlclientQueryCmd -e "UPDATE metadata_index SET shared_root_id = '$SharedFolderId' WHERE id IN ($SharedMetadataIndexIds)"
		done
	done
}

copy_additional_attrs() {
	$SqlclientQueryCmd -e "SELECT name FROM user_info WHERE status = 'active'" | while read -r UserId; do
		RootId=$($SqlclientQueryCmd -e "SELECT id FROM metadata_index WHERE parent_id = '' AND name = '$UserId' LIMIT 1")
		MyBackupDataId=$($SqlclientQueryCmd -e "SELECT id FROM metadata_index WHERE parent_id = '$RootId' AND name = 'my backup data' LIMIT 1")
		
		BackupMetadataIndexIds=$(fetch_metadata_index_id_by_parent_id "$MyBackupDataId" "" | tr '\n' ',' | sed 's/,$//')
		$SqlclientQueryCmd -e "SELECT id, folders_count, files_count, size, params FROM metadata_index WHERE id IN ($BackupMetadataIndexIds)" | while read -r MetadataIndexId FoldersCount FilesCount Size Params; do
			Width=0
			Height=0
			if [ ! -z "$Params" ]; then
				Width="$(echo $Params | sed 's/.*\"img_width\":\"\{0,1\}\([[:digit:]]\{1,\}\)\"\{0,1\}.*/\1/g')"
				Height="$(echo $Params | sed 's/.*\"img_height\":\"\{0,1\}\([[:digit:]]\{1,\}\)\"\{0,1\}.*/\1/g')"
			fi
			$SqlclientQueryCmd -e "INSERT INTO metadata_index_additional_attr (metadata_index_id, is_backup, folders_count, files_count, total_size, width, height) \
				VALUES ('$MetadataIndexId', 1, $FoldersCount, $FilesCount, $Size, $Width, $Height)"
		done
		
		FormalMetadataIndexIds=$(fetch_metadata_index_id_by_parent_id "$RootId" "$MyBackupDataId" | tr '\n' ',' | sed 's/,$//')
		$SqlclientQueryCmd -e "SELECT id, folders_count, files_count, size, params FROM metadata_index WHERE id IN ($FormalMetadataIndexIds)" | while read -r MetadataIndexId FoldersCount FilesCount Size Params; do
			Width=0
			Height=0
			if [ ! -z "$Params" ]; then
				Width="$(echo $Params | sed 's/.*\"img_width\":\"\{0,1\}\([[:digit:]]\{1,\}\)\"\{0,1\}.*/\1/g')"
				Height="$(echo $Params | sed 's/.*\"img_height\":\"\{0,1\}\([[:digit:]]\{1,\}\)\"\{0,1\}.*/\1/g')"
			fi
			$SqlclientQueryCmd -e "INSERT INTO metadata_index_additional_attr (metadata_index_id, is_backup, folders_count, files_count, total_size, width, height) \
				VALUES ('$MetadataIndexId', 0, $FoldersCount, $FilesCount, $Size, $Width, $Height)"
		done
	done
	
	$SqlclientQueryCmd -e "UPDATE metadata_index SET size = 0 WHERE is_folder = 1"
}

move_my_backup_data() {
	$SqlclientQueryCmd -e "SELECT name FROM user_info WHERE status = 'active'" | while read -r UserId; do
		RootId=$($SqlclientQueryCmd -e "SELECT id FROM metadata_index WHERE parent_id = '' AND name = '$UserId' LIMIT 1")
		MyBackupDataId=$($SqlclientQueryCmd -e "SELECT id FROM metadata_index WHERE parent_id = '$RootId' AND name = 'my backup data' LIMIT 1")
		BackupMetadataIndexIds=$(fetch_metadata_index_id_by_parent_id "$MyBackupDataId" "" | tr '\n' ',' | sed 's/,$//')
		
		$SqlclientQueryCmd -e "SELECT id FROM metadata_index WHERE id IN ($BackupMetadataIndexIds)" | while read -r MetadataIndexId; do
			echo 'TODO'
		done
	done
}

##
## migrate start
##
$SqlclientQueryCmd < "$TargetDirPath"/migrate_erlang_to_java.sql

## migrate_default_backup_setting_group
## migrate_default_backup_setting
## migrate_user_backup_setting

## migrate_device_info

## migrate_global_folder_icon_group
## migrate_global_folder_icon

## migrate_metadata_index_and_backup_metadata_index
migrate_metadata_index_additional_attr
migrate_collaborate
migrate_collaborate_member
migrate_read_history
migrate_file_processing_status
migrate_reader_file_metadata
migrate_reader_file_metadata_detail
migrate_share_link
migrate_tbl_profile
migrate_tbl_profile_property
migrate_text_view_setting

migrate_mobile_folders
migrate_user_info

## clean temp dir
echo 'clean temp dir...'
## rm -rf "$WorkTempDir"
