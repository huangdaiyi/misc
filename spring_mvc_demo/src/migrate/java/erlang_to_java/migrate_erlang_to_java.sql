DROP VIEW `collaborate_view`;

CREATE TABLE `backup_metadata_index` (
  `id` char(36) NOT NULL,
  `parent_id` char(36) NOT NULL DEFAULT '',
  `name` varchar(200) NOT NULL DEFAULT '',
  `is_folder` bit(1) NOT NULL DEFAULT b'1',
  `sort_priority` int(11) NOT NULL DEFAULT '0',
  `size` bigint(20) NOT NULL DEFAULT '0',
  `origin_name` varchar(200) NOT NULL DEFAULT '',
  `modified_at` bigint(20) NOT NULL DEFAULT '0',
  `modified_by` varchar(45) NOT NULL DEFAULT '',
  `device_uid` varchar(100) NOT NULL DEFAULT '',
  `link_path` varchar(5000) NOT NULL DEFAULT '',
  `note` varchar(5000) NOT NULL DEFAULT '',
  `params` varchar(5000) NOT NULL DEFAULT '',
  `icon_block_id` varchar(70) NOT NULL DEFAULT '',
  `is_encrypted` bit(1) NOT NULL DEFAULT b'0',
  `created_at` bigint(20) NOT NULL DEFAULT '0',
  `created_by` varchar(45) NOT NULL DEFAULT '',
  `block_id` varchar(96) NOT NULL DEFAULT '',
  `type` varchar(20) NOT NULL DEFAULT 'normal',
  `owner_id` varchar(45) NOT NULL DEFAULT '',
  `original_index_id` char(36) NOT NULL DEFAULT '0',
  `backup_no` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `idx_device_id` (`device_uid`) USING BTREE,
  KEY `idx_metadata_index_link_path_id` (`link_path`(255),`id`),
  KEY `idx_metadata_index_parent_id_name_is_folder_device_uid_link_path` (`parent_id`,`name`,`is_folder`,`device_uid`,`link_path`(255)),
  KEY `idx_backup_metadata_index_original_index_id_parent_id_id` (`original_index_id`,`parent_id`,`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `metadata_index_additional_attr` (
  `metadata_index_id` char(36) NOT NULL,
  `is_backup` bit(1) NOT NULL,
  `folders_count` int(11) DEFAULT '0',
  `files_count` int(11) DEFAULT '0',
  `total_size` bigint(20) DEFAULT '0',
  `width` int(11) DEFAULT '0',
  `height` int(11) DEFAULT '0',
  PRIMARY KEY (`metadata_index_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
