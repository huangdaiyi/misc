package service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import model.BackupMetadata;
import model.BackupMetadata4Delete;
import model.BackupMetadata4Modify;
import model.BaseMetadata;
import model.CountAffected;
import model.DefaultBackupSetting;
import model.MetaAffectResult;
import model.Metadata;
import model.MetadataAttr;

import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import service.BackupMetadataService;
import service.CountService;
import service.MetadataAttrService;
import utils.MetadataUtils;
import utils.StringUtils;
import utils.TypeConvert;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import constants.AffectTag;
import constants.HttpStatus;
import dao.BackupMetadataDao;
import dao.ExtensionBackupDao;
import dao.MetadataDao;
import exception.MetadataException;
import factory.MetadataFactory;

@Service
public class BackupMetadataServiceImpl implements BackupMetadataService {

	@Autowired
	private BackupMetadataDao backupMetadataDao;

	@Autowired
	private CountService countService;

	@Autowired
	private MetadataDao metadataDao;

	@Autowired
	private MetadataAttrService metadataAttrService;

	@Autowired
	private ExtensionBackupDao extensionBackupDao;

	@Autowired
	private MetadataFactory metadataFactory;

	@Override
	@Transactional
	public void createSingleBackup(Metadata metadata, String userId) {
		if (null == metadata) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		if (!metadata.isFolder() && getBackupCount(metadata) < 1) {
			return;
		}

		String parentId = metadata.getParentId();

		BackupMetadata parentBackup = null;
		BackupMetadata root = backupMetadataDao.getRoot(userId);

		if (StringUtils.isNullOrEmpty(parentId)) {
			parentBackup = root;
		} else {
			parentBackup = backupMetadataDao.getLatestBackup(parentId);
		}

		if (null == parentBackup) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		// delete old backup if exist
		deleteFileOldBackups(metadata);

		BackupMetadata backup = TypeConvert.convert(metadata, StringUtils.getUUID(), parentBackup.getId());
		backupMetadataDao.createFileBuckup(backup);
		MetadataAttr attr = new MetadataAttr(backup.getId(), true);
		attr.setWidth(metadata.getWidth());
		attr.setHeight(metadata.getHeight());
		CountAffected countAffected = null;
		if (backup.isFolder()) {
			countAffected = new CountAffected(backup.getParentId(), 0, 1, 0, 0, 0, 0);
		} else {
			countAffected = new CountAffected(backup.getParentId(), 1, 0, backup.getTotalSize(), 0, 0, 0);
			// file default is 1
			attr.setFilesCount(1);
			attr.setTotalSize(backup.getTotalSize());
		}

		metadataAttrService.createMetadataAttr(attr);
		countService.updateAllParentCountBackup(Arrays.asList(countAffected));
		// totalSize = metadata.getTotalSize() - totalSize;
		// metadataAttrService.updateRootSize(root.getId(), totalSize);

	}

	@Override
	@Transactional
	public List<BackupMetadata> backupMetadata(Metadata metadata, String ownerId) {

		if (null == metadata || StringUtils.isNullOrEmpty(metadata.getId()) || StringUtils.isNullOrEmpty(metadata.getParentId())
				|| StringUtils.isNullOrEmpty(ownerId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		// check
		String parentId = metadata.getParentId();
		BackupMetadata parentBackup = null;
		BackupMetadata root = backupMetadataDao.getRoot(ownerId);
		if (StringUtils.isNullOrEmpty(parentId)) {
			parentBackup = root;
		} else {
			BackupMetadata tempBackup = backupMetadataDao.getLatestBackup(parentId);
			if (null == tempBackup) {
				throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
			}
			parentBackup = tempBackup;
		}

		if (null == parentBackup) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		return backupNotWithAttr(Arrays.asList(metadata), parentId, parentBackup, root.getId());
	}

	@Override
	public List<BackupMetadata> backupMetadata(List<Metadata> metadatas, String ownerId, String firstExistParentId)
			throws MetadataException {
		// for (Metadata item : metadatas) {
		// StringUtils.writeJSON(item);
		// }
		if (StringUtils.isNullOrEmpty(metadatas) || StringUtils.isNullOrEmpty(ownerId) || StringUtils.isNullOrEmpty(firstExistParentId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		// check exist
		String root = backupMetadataDao.getRootId(ownerId);
		if (StringUtils.isNullOrEmpty(root)) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		BackupMetadata parentBackup = backupMetadataDao.getLatestBackup(firstExistParentId);

		if (null == parentBackup) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		return backupNotWithAttr(metadatas, firstExistParentId, parentBackup, root);

	}

	/***
	 * Use this method only under the root metadata; such as reset
	 ***/
	@Override
	@Transactional
	public List<BackupMetadata> backupMetadata(List<Metadata> metadatas, String ownerId) throws MetadataException {

		if (StringUtils.isNullOrEmpty(metadatas) || StringUtils.isNullOrEmpty(ownerId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		// check root
		BackupMetadata backupRoot = backupMetadataDao.getRoot(ownerId);
		if (null == backupRoot) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		return backupNotWithAttr(metadatas, backupRoot.getOriginalIndexId(), backupRoot, backupRoot.getId());
	}

	@Override
	public List<BackupMetadata> ExistBackupName(String metadataName, String backupParentId, String sourePath) {
		if (StringUtils.isNullOrEmpty(metadataName) || StringUtils.isNullOrEmpty(backupParentId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return backupMetadataDao.ExistBackupName(metadataName, backupParentId, sourePath);
	}

	@Override
	@Transactional
	public BackupMetadata registerRootBackup(BackupMetadata root) {
		if (root == null) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		if (StringUtils.isNullOrEmpty(root.getId())) {
			root.setId(StringUtils.getUUID());
		}

		root.setParentId("");

		metadataAttrService.createMetadataAttr(new MetadataAttr(root.getId(), true));
		return backupMetadataDao.createFolderBackup(root);
	}

	@Override
	@Transactional
	public void deleteBackup(String backupMetadataId) throws MetadataException {

		if (StringUtils.isNullOrEmpty(backupMetadataId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		List<String> parentIds = getParentIds(backupMetadataId);

		List<MetadataAttr> metadataAttrs = new ArrayList<MetadataAttr>();
		for (String pid : parentIds) {
			metadataAttrs.add(new MetadataAttr(pid, true, -1, 0, 0L, 0, 0));
		}

		metadataAttrService.deleteMetadataAttr(backupMetadataId);
		metadataAttrService.batchUpdateMetadataAttrOnOrigin(metadataAttrs);
		backupMetadataDao.deleteBackupById(backupMetadataId);
	}

	@Override
	@Transactional
	public void deleteFolderBackup(String ownerId, String folderId) {
		if (StringUtils.isNullOrEmpty(ownerId) || StringUtils.isNullOrEmpty(folderId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		clearFolder(ownerId, folderId);
	}

	@Override
	@Transactional
	public void deleteFolderAllBackup(String ownerId, String folderId) {
		if (StringUtils.isNullOrEmpty(ownerId) || StringUtils.isNullOrEmpty(folderId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		recursiveClearFolder(ownerId, folderId);
	}

	@Override
	@Transactional
	public void deleteAllBackup(String ownerId) {
		if (StringUtils.isNullOrEmpty(ownerId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		List<BackupMetadata> unbackupMetadatas = backupMetadataDao.getAllUnbackupMetadatas(ownerId);
		deleteUnBackup(unbackupMetadatas);
	}

	@Override
	public List<String> getParentIds(String id) {

		if (StringUtils.isNullOrEmpty(id)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		List<String> parents = new ArrayList<String>();
		String parentId = null;
		do {
			parentId = backupMetadataDao.getBackupParentId(id);

			if (parents.contains(parentId)) {
				// metadata data error log ;
				throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
			}

			if (StringUtils.isNullOrEmpty(parentId)) {
				break;
			}
			parents.add(parentId);
			id = parentId;
		} while (true);

		return parents;
	}

	@Override
	@Transactional
	public BackupMetadata updateBackup(Metadata metadata) {
		if (metadata == null || StringUtils.isNullOrEmpty(metadata.getId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		BackupMetadata backupMetadata = backupMetadataDao.getLatestBackup(metadata.getId());
		if (backupMetadata == null) {
			return null;
		}

		// update field ...
		backupMetadata.setBlockId(metadata.getBlockId());
		backupMetadata.setModifiedAt(metadata.getModifiedAt());
		backupMetadata.setSize(metadata.getSize());
		backupMetadata.setSortPriority(metadata.getSortPriority());

		long size = metadata.getSize() - backupMetadata.getSize();
		if (size != 0 || metadata.getWidth() != backupMetadata.getWidth() || metadata.getHeight() != backupMetadata.getHeight()) {

			MetadataAttr attr = new MetadataAttr();
			attr.setMetadataIndexId(backupMetadata.getId());
			attr.setWidth(metadata.getWidth());
			attr.setHeight(metadata.getWidth());
			attr.setTotalSize(size);
			metadataAttrService.updateMetadataAttrOnOriginWithAspec(attr);
		}

		return backupMetadataDao.updateBackup(backupMetadata);
	}

	@Override
	public void renameBackup(Metadata metadata) {
		if (metadata == null || StringUtils.isNullOrEmpty(metadata.getId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		List<BackupMetadata> backupMetadatas = backupMetadataDao.getBackupByOriginalId(metadata.getId());
		if (StringUtils.isNullOrEmpty(backupMetadatas)) {
			return;
		}

		for (BackupMetadata backupMetadata : backupMetadatas) {
			if (!metadata.isFolder()) {
				backupMetadata.setOriginName(metadata.getOriginName());
				backupMetadata.setName(metadata.getName());
				backupMetadata.setFullSourcePath(metadata.getFullSourcePath());

			} else {
				backupMetadata.setOriginName(metadata.getOriginName());
				backupMetadata.setName(metadata.getName());
			}
			backupMetadata.setModifiedAt(metadata.getModifiedAt());
		}

		backupMetadataDao.batchUpdateBackup(backupMetadatas);
	}

	@Override
	public String getBackupIndexByPath(String fullPath, String ownerId) {

		if (StringUtils.isNullOrEmpty(ownerId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		String rootId = backupMetadataDao.getRootId(ownerId);

		// not register root
		if (StringUtils.isNullOrEmpty(rootId)) {
			throw new MetadataException(HttpStatus.DIR_NOT_EXIST);
		}

		if (StringUtils.isNullOrEmpty(fullPath) || fullPath.equals("/")) {
			return rootId;
		}

		String[] names = fullPath.split("/");
		String parentId = rootId;

		for (String name : names) {

			if (StringUtils.isNullOrEmpty(name)) {
				continue;
			}

			parentId = backupMetadataDao.getBackupIndexId(ownerId, name, parentId);
			if (StringUtils.isNullOrEmpty(parentId)) {
				throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
			}

		}

		return parentId;
	}

	// public List<BackupMetadata> getAllSubBackupMetadata(BackupMetadata
	// parent) {
	// List<BackupMetadata> backupMetadatas =
	// backupMetadataDao.getBackupMetdatasByCriteria(parent.getId(),
	// parent.getName(), parent.getDeviceUid(), parent.getFullSourcePath());
	// int i = 0;
	// BackupMetadata temBackup = null;
	// while (i < backupMetadatas.size()) {
	// temBackup = backupMetadatas.get(i++);
	// if (!temBackup.isFolder()) {
	// continue;
	// }
	// backupMetadatas.addAll(backupMetadataDao.getBackupMetdatasByCriteria(temBackup.getId(),
	// temBackup.getName(), temBackup.getDeviceUid(),
	// temBackup.getFullSourcePath()));
	// }
	// return backupMetadatas;
	// }

	/********** private method **************/

	public List<BackupMetadata> backupNotWithAttr(List<Metadata> metadatas, String parentId, BackupMetadata parentBackup, String rootId) {
		
		// get all old backup
		final List<BackupMetadata> allBackupMetadatas = getAllSubFoldersAndFiles(parentBackup);
		allBackupMetadatas.add(parentBackup);
		
		Iterable<BackupMetadata> unbackupMetadata = Lists.newArrayList(Iterables.filter(allBackupMetadatas, new Predicate<BackupMetadata>() {

			@Override
			public boolean apply(BackupMetadata backupItem) {
				return backupItem.getOriginalIndexId().equals("");
			}

		}));
		// backupMetadataDao.getUnBackupMetdatasByParentIds(backupParentIds);
		// final List<BackupMetadata> allBackupMetadatas =
		// getAllSubBackupMetadata(parentBackup);
		// backupMetadataDao.getBackups(metadataIds);
		String buckupId = parentBackup.getId();
		List<CountAffected> affectMetadatas = new ArrayList<CountAffected>();

		List<Metadata> folders = MetadataUtils.findAllFolders(metadatas);
		List<BackupMetadata> creations = new ArrayList<BackupMetadata>();
		List<String> deleteBackups = new ArrayList<String>();
		if (folders.size() > 0) {

			List<Metadata> notExistsFolderList = getNotExistsFolders(allBackupMetadatas, folders);

			if (notExistsFolderList.size() > 0) {
				List<BackupMetadata> newBackups = mergeBackupFolders(notExistsFolderList, buckupId, parentId, allBackupMetadatas,
						affectMetadatas, unbackupMetadata, deleteBackups);
				creations.addAll(newBackups);
				allBackupMetadatas.addAll(newBackups);
			}

		}

		List<Metadata> files = MetadataUtils.findAllFiles(metadatas);
		List<BackupMetadata4Modify> modifies = new ArrayList<BackupMetadata4Modify>();
		List<BackupMetadata> backupMetadatas = null;
		// List<String> tempDelMetadatas = new ArrayList<String>(50);

		if (files.size() > 0) {

			String prevMetaId = parentId, curBackupPId = buckupId, curId = null;
			int backupCount = 0, backupedCount = 0;
			CountAffected tempAffected = getCountAffected(affectMetadatas, curBackupPId);
			BackupMetadata tempFile = null;
			for (Metadata file : files) {

				// encrypt file not backup
				if (file.isEncrypted()) {
					continue;
				}

				curId = StringUtils.getUUID();

				// if files have sequence less getBackupRootId
				if (!file.getParentId().equals(prevMetaId)) {
					curBackupPId = getBackupRootId(file.getParentId(), allBackupMetadatas);
					if (null == curBackupPId) {
						throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
					}
					prevMetaId = file.getParentId();
					tempAffected = getCountAffected(affectMetadatas, curBackupPId);
				}

				// Notice backup count
				backupCount = file.getBackupCount() == -1 ? 1 : file.getBackupCount();

				backupMetadatas = getBackupByOriginId(allBackupMetadatas, file.getId());
				backupedCount = backupMetadatas.size();

				// add create metadata
				tempFile = TypeConvert.convert(file, curId, curBackupPId);
				deleteOldUnbackup(unbackupMetadata, tempFile, tempAffected, deleteBackups);
				creations.add(TypeConvert.convert(file, curId, curBackupPId));
				tempAffected.addFileCount(1);
				tempAffected.addSize(file.getTotalSize());
				if (backupedCount >= backupCount && backupedCount > 0) {
					tempAffected.addSize(deleteBackupNotWithAttr(backupMetadatas, deleteBackups, backupCount - 1, backupedCount));
					tempAffected.addFileCount(backupMetadatas.size() - backupedCount);

				}
				if (backupMetadatas.size() > 0) {
					modifies.addAll(Collections2.transform(backupMetadatas, new Function<BackupMetadata, BackupMetadata4Modify>() {

						@Override
						public BackupMetadata4Modify apply(BackupMetadata backup) {
							return new BackupMetadata4Modify(backup.getId(), backup.getOriginName(), backup.getBackupNo() + 1);
						}

					}));
				}

			}

		}

		updateDbWithAttr(creations, modifies, deleteBackups, rootId, affectMetadatas);

		return creations;
	}

	// private List<String> getAllParentIds(List<BackupMetadata> backups){
	//
	//
	// return Lists.newArrayList( Iterables.transform(Iterables.filter(backups,
	// new Predicate<BackupMetadata>(){
	// @Override
	// public boolean apply(BackupMetadata item) {
	// return item.isFolder();
	// }
	//
	// }), new Function<BackupMetadata, String>(){
	//
	// @Override
	// public String apply(BackupMetadata item2) {
	// return item2.getId();
	// }
	//
	// }));
	// }

	private void updateDbWithAttr(List<BackupMetadata> creations, List<BackupMetadata4Modify> modifies, List<String> deleteBackups,
			String rootId, List<CountAffected> affectMetadatas) {
		if (creations.size() > 0) {
			List<MetadataAttr> creationAttrs = Lists.newArrayList(Iterables.transform(creations,
					new Function<BackupMetadata, MetadataAttr>() {

						@Override
						public MetadataAttr apply(BackupMetadata tempBackup) {
							return new MetadataAttr(tempBackup.getId(), true, 0, tempBackup.isFolder() ? 0 : 1, tempBackup.isFolder() ? 0
									: tempBackup.getTotalSize(), tempBackup.getWidth(), tempBackup.getHeight());
						}

					}));

			backupMetadataDao.batchFileCreateBackup(creations);
			metadataAttrService.batchCreateMetadataAttr(creationAttrs);
		}

		if (deleteBackups.size() > 0) {
			backupMetadataDao.deleteBackupByIds(deleteBackups);
			metadataAttrService.deleteMetadataAttrs(deleteBackups);
		}

		if (modifies.size() > 0) {
			backupMetadataDao.bathUpdateBackupNo(modifies);
		}

		if (affectMetadatas.size() > 0) {
			countService.updateAllParentCountBackup(affectMetadatas);
		}

		// metadataAttrService.updateRootSize(rootId, totalSize);
	}

	// if parentId is null or empty string, will be found roots, but all
	// 'metadatas' should have a common parent
	@SuppressWarnings("unused")
	private long backup(List<Metadata> metadatas, String parentId, String buckupId) {

		List<String> metadataIds = Lists.newArrayList(Iterables.transform(metadatas, new Function<Metadata, String>() {

			@Override
			public String apply(Metadata item) {
				return item.getId();
			}

		}));

		// get all old backup
		final List<BackupMetadata> allBackupMetadatas = backupMetadataDao.getBackups(metadataIds);

		List<MetaAffectResult> affectResults = buildResult(Iterables.filter(allBackupMetadatas, new Predicate<BackupMetadata>() {
			@Override
			public boolean apply(BackupMetadata item) {
				return item.isFolder();
			}

		}));

		List<Metadata> folders = MetadataUtils.findAllFolders(metadatas);
		List<BackupMetadata> creations = new ArrayList<BackupMetadata>();
		if (folders.size() > 0) {

			List<Metadata> notExistsFolderList = getNotExistsFolders(allBackupMetadatas, folders);

			if (notExistsFolderList.size() > 0) {
				if (StringUtils.isNullOrEmpty(parentId)) {
					creations.addAll(mergeBackupFolders(notExistsFolderList, allBackupMetadatas, buckupId));
				} else {
					// creations.addAll(mergeBackupFolders(notExistsFolderList,
					// buckupId, parentId, allBackupMetadatas));
				}

				affectResults.addAll(buildResult(creations));

				for (BackupMetadata creation : creations) {
					calculateCount(affectResults, creation, true);
				}

			}

		}

		List<Metadata> files = MetadataUtils.findAllFiles(metadatas);
		List<BackupMetadata> backupMetadatas = null;
		List<BackupMetadata4Modify> modifies = new ArrayList<BackupMetadata4Modify>();
		List<BackupMetadata4Delete> deletions = new ArrayList<BackupMetadata4Delete>();

		if (files.size() > 0) {

			List<BackupMetadata> tempBackupMetadatas = mergeBackupFiles(files, allBackupMetadatas, buckupId, "");
			creations.addAll(tempBackupMetadatas);
			affectResults.addAll(buildResult(tempBackupMetadatas));
			int backupCount = 0, backupedCount = 0;

			for (final Metadata file : files) {
				// backupCount = getBackupCount(file, metadatas);
				// Notice backup count
				backupCount = file.getBackupCount() == -1 ? 1 : file.getBackupCount();
				backupMetadatas = getBackupByOriginId(allBackupMetadatas, file.getId());
				backupedCount = backupMetadatas.size();
				if (backupedCount >= backupCount && backupedCount > 0) {

					deletions.addAll(deleteBackup(affectResults, backupMetadatas, backupCount - 1, backupedCount));
				}

				calculateCount(affectResults, Iterables.find(tempBackupMetadatas, new Predicate<BackupMetadata>() {

					@Override
					public boolean apply(BackupMetadata item) {
						return file.getId().equals(item.getOriginalIndexId());
					}

				}), true);

				backupedCount = backupMetadatas.size();
				if (backupedCount > 0) {
					modifies.addAll(Collections2.transform(backupMetadatas, new Function<BackupMetadata, BackupMetadata4Modify>() {

						@Override
						public BackupMetadata4Modify apply(BackupMetadata backup) {
							return new BackupMetadata4Modify(backup.getId(), backup.getOriginName(), backup.getBackupNo() + 1);
						}

					}));
				}

			}

		}

		List<String> deletionAttrIds = new ArrayList<String>();
		List<MetadataAttr> creationAttrs = new ArrayList<MetadataAttr>(), modifyAttrs = new ArrayList<MetadataAttr>();

		long size = parseResult(affectResults, deletionAttrIds, creationAttrs, modifyAttrs);

		updateDb(deletions, modifies, creations, deletionAttrIds, creationAttrs, modifyAttrs);

		return size;
	}

	public void deleteFileOldBackups(Metadata metadata) {
		BackupMetadata backupParent = backupMetadataDao.getLatestBackup(metadata.getParentId());

		// parent not exist ---> file not exists backup file
		if (backupParent == null) {
			return;
		}

		String parentId = backupParent.getId();

		List<BackupMetadata> backupDatas = ExistBackupName(metadata.getName(), backupParent.getId(), metadata.getSourcePath());

		// has not old backup
		if (StringUtils.isNullOrEmpty(backupDatas)) {
			return;
		}

		List<String> backupMetadataIds = Lists.newArrayList(Iterables.transform(backupDatas, new Function<BackupMetadata, String>() {
			@Override
			public String apply(BackupMetadata backup) {
				return backup.getId();
			}

		}));
		long totalSize = 0L;
		for (BackupMetadata backupData : backupDatas) {
			totalSize = totalSize + backupData.getSize();
		}

		List<String> parentIds = getParentIds(parentId);
		parentIds.add(parentId);

		List<MetadataAttr> metadataAttrs = new ArrayList<MetadataAttr>();
		int effectCount = -backupDatas.size();
		for (String pid : parentIds) {
			metadataAttrs.add(new MetadataAttr(pid, true, 0, effectCount, -totalSize, 0, 0));
		}

		metadataAttrService.deleteMetadataAttrs(backupMetadataIds);
		metadataAttrService.batchUpdateMetadataAttrOnOrigin(metadataAttrs);
		backupMetadataDao.deleteBackupByIds(backupMetadataIds);
		// return totalSize;

	}

	@Override
	public int getBackupCount(Metadata file) {
		if (file == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}

		int backupCount = file.getBackupCount();
		if (backupCount != -1) {
			return backupCount;
		}

		List<Metadata> allUpperLevelMetadatas = metadataFactory.getAllUpperLevelMetadatas(file);
		return getBackupCount(file, allUpperLevelMetadatas);
	}

	@Override
	public int getBackupCount(Metadata file, List<Metadata> metadatas) {
		int backupCount = file.getBackupCount();
		if (backupCount != -1) {
			return backupCount;
		}

		backupCount = getBackupCountByExtension(file.getOwnerId(), Arrays.asList(file)).get(0).getBackupCount();
		if (backupCount != -1) {
			return backupCount;
		}

		String tempId = file.getId();
		Metadata tempMetadata = null;
		// -1 get parent count
		int maxLoop = metadatas.size();
		for (int i = 0; i < maxLoop; i++) {
			tempMetadata = findMetadata(metadatas, tempId);
			if (null == tempMetadata) {
				// data error
				backupCount = 1;
				break;
			}

			backupCount = tempMetadata.getBackupCount();
			if (backupCount != -1) {
				break;
			}

			tempId = tempMetadata.getParentId();
		}

		return backupCount;

	}

	@Override
	public List<Metadata> getBackupCountByExtension(String ownerId, List<Metadata> metadatas) {
		boolean IsIncludeFile = false;
		for (Metadata metadata : metadatas) {
			if (!metadata.isFolder()) {
				IsIncludeFile = true;
			}
		}
		if (!IsIncludeFile) {
			return metadatas;
		}

		List<DefaultBackupSetting> defaultExtensionBackups = extensionBackupDao.getExtensionBackup(ownerId);
		if (defaultExtensionBackups.isEmpty()) {
			return metadatas;
		}
		for (Metadata metadata : metadatas) {
			if (metadata.getBackupCount() == -1 && metadata.getName().indexOf(".") > 0 && !metadata.isFolder()) {
				for (DefaultBackupSetting defaultExtensionBackup : defaultExtensionBackups) {
					if (defaultExtensionBackup.getExtension().equals(FilenameUtils.getExtension(metadata.getName()))) {
						metadata.setBackupCount(defaultExtensionBackup.getBackupCount());
					}
				}
			}
		}

		return metadatas;
	}

	private Metadata findMetadata(List<Metadata> metadatas, final String metadataId) {
		Metadata tempMetadata;
		tempMetadata = Iterables.find(metadatas, new Predicate<Metadata>() {

			@Override
			public boolean apply(Metadata metadataItem) {
				return metadataItem.getId().equals(metadataId);
			}

		}, null);
		return tempMetadata;
	}

	private void updateDb(List<BackupMetadata4Delete> deletions, List<BackupMetadata4Modify> modifies, List<BackupMetadata> creations,
			List<String> deletionAttrIds, List<MetadataAttr> creationAttrs, List<MetadataAttr> modifyAttrs) {

		if (deletions.size() > 0) {
			backupMetadataDao.batchDeleteBackup(deletions);
		}

		if (modifies.size() > 0) {
			backupMetadataDao.bathUpdateBackupNo(modifies);
		}

		if (creations.size() > 0) {
			backupMetadataDao.batchFileCreateBackup(creations);
		}

		if (deletionAttrIds.size() > 0) {
			metadataAttrService.deleteMetadataAttrs(deletionAttrIds);
		}

		if (modifyAttrs.size() > 0) {
			metadataAttrService.batchUpdateMetadataAttrOnOriginWithAspect(modifyAttrs);
		}

		if (creationAttrs.size() > 0) {
			metadataAttrService.batchCreateMetadataAttr(creationAttrs);
		}
	}

	private long parseResult(List<MetaAffectResult> affectResults, List<String> deletionAttrIds, List<MetadataAttr> creationAttrs,
			List<MetadataAttr> modifyAttrs) {

		long size = 0L;
		for (MetaAffectResult affectResult : affectResults) {
			if (!affectResult.isFolder()) {
				size += affectResult.getSize();
			}

			if (affectResult.getTag() == AffectTag.CREATE) {
				creationAttrs.add(affectResult.toMetadataAdditionalAttr(true));
			} else if (affectResult.getTag() == AffectTag.REMOVE) {
				deletionAttrIds.add(affectResult.getMetadataId());
			}

			if (affectResult.isAffect()) {
				modifyAttrs.add(affectResult.toMetadataAdditionalAttr(true));
			}

		}

		return size;
	}

	private long deleteBackupNotWithAttr(List<BackupMetadata> backupMetadatas, List<String> deleteBackupIds, int begin, int end) {

		Collections.sort(backupMetadatas, new Comparator<BackupMetadata>() {

			@Override
			public int compare(BackupMetadata backupMetadata1, BackupMetadata backupMetadata2) {
				return backupMetadata1.getBackupNo() - backupMetadata2.getBackupNo();
			}

		});

		long deleteSize = 0L;

		// List<String> deleteBackupIds = new ArrayList<String>();
		List<BackupMetadata> backupView = backupMetadatas.subList(begin, end);
		for (BackupMetadata backup : backupView) {
			deleteBackupIds.add(backup.getId());
			deleteSize += backup.getTotalSize();
		}

		backupMetadatas.removeAll(backupView);

		return deleteSize;
	}

	private Collection<BackupMetadata4Delete> deleteBackup(List<MetaAffectResult> effectResults, List<BackupMetadata> backupMetadatas,
			int begin, int end) {

		Collections.sort(backupMetadatas, new Comparator<BackupMetadata>() {

			@Override
			public int compare(BackupMetadata backupMetadata1, BackupMetadata backupMetadata2) {
				return backupMetadata1.getBackupNo() - backupMetadata2.getBackupNo();
			}

		});

		List<BackupMetadata> deleteBackupFiles = new ArrayList<BackupMetadata>(), backupView = backupMetadatas.subList(begin, end);
		for (BackupMetadata backup : backupView) {
			deleteBackupFiles.add(backup);
		}

		backupMetadatas.removeAll(backupView);

		effectResults.addAll(buildResult(deleteBackupFiles));
		for (BackupMetadata backupMetadata : deleteBackupFiles) {
			for (MetaAffectResult effectResult : effectResults) {
				if (effectResult.getMetadataId().equals(backupMetadata.getId())) {
					calculateCount(effectResults, backupMetadata, false);
					// effectReult.addSize(backupMetadata.getSize());
					break;
				}
			}
		}

		return Collections2.transform(deleteBackupFiles, new Function<BackupMetadata, BackupMetadata4Delete>() {

			@Override
			public BackupMetadata4Delete apply(BackupMetadata backup) {
				return new BackupMetadata4Delete(backup.getOriginalIndexId(), backup.getBackupNo());
			}

		});
	}

	private List<MetaAffectResult> buildResult(Iterable<BackupMetadata> metadatas) {
		return Lists.newArrayList(Iterables.transform(metadatas, new Function<BackupMetadata, MetaAffectResult>() {

			@Override
			public MetaAffectResult apply(BackupMetadata backup) {
				return new MetaAffectResult(backup.getId(), backup.getParentId(), backup.isFolder(), backup.getWidth(), backup.getHeight());
			}
		}));
	}

	private void calculateCount(List<MetaAffectResult> backupResults, final BackupMetadata backupMetadata, Boolean isCreate) {

		String parentId = backupMetadata.getParentId();
		MetaAffectResult temp = null, self = Iterables.find(backupResults, new Predicate<MetaAffectResult>() {
			@Override
			public boolean apply(MetaAffectResult item) {
				return item.getMetadataId().equals(backupMetadata.getId());
			}

		});

		int maxLoop = backupResults.size(), loop = 0;
		int count = isCreate ? 1 : -1;
		long size = 0L;
		if (isCreate) {
			count = 1;
			size = backupMetadata.getSize();
			self.setTag(AffectTag.CREATE);
		} else {
			count = -1;
			size = -backupMetadata.getSize();
			self.setTag(AffectTag.REMOVE);
		}

		// add self
		if (!self.isFolder()) {
			self.addSize(size);
		}

		// add other
		do {
			temp = findParent(backupResults, parentId);
			// temp.addCount(count, self.isFolder());

			if (loop > maxLoop || null == temp) {
				throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
			}

			if (self.isFolder()) {
				temp.addFolderCount(count);
			} else {
				temp.addFileCount(count);
				temp.addSize(size);
			}

			parentId = temp.getParentId();
			if (StringUtils.isNullOrEmpty(parentId)) {
				break;
			}

			loop++;
		} while (true);

	}

	private MetaAffectResult findParent(Iterable<MetaAffectResult> backupResults, final String parentId) {
		return Iterables.find(backupResults, new Predicate<MetaAffectResult>() {
			@Override
			public boolean apply(MetaAffectResult item) {
				return item.getMetadataId().equals(parentId);
			}

		}, null);
	}

	private List<BackupMetadata> mergeBackupFolders(List<Metadata> metadatas, List<BackupMetadata> allBackupMetadatas, String backupRootId) {
		List<BackupMetadata> newBackupMetadatas = new ArrayList<BackupMetadata>();
		String parentId = null;
		List<Metadata> roots = findRoots(metadatas);
		metadatas.removeAll(roots);
		for (Metadata root : roots) {
			parentId = getBackupRootId(root.getParentId(), allBackupMetadatas);
			if (parentId == null) {
				parentId = backupRootId;
			}
			buidTree(metadatas, root, allBackupMetadatas, newBackupMetadatas, parentId);
		}

		return newBackupMetadatas;
	}

	// private List<BackupMetadata> mergeBackupFolders(List<Metadata> metadatas,
	// List<BackupMetadata> allBackupMetadatas, BackupMetadata parent, String
	// firstExistParentId) {
	// Iterable<Metadata> subRoots = findNextRoots(metadatas,
	// firstExistParentId);
	// List<BackupMetadata> newBackupMetadatas = new
	// ArrayList<BackupMetadata>();
	// Map<String,String> folderMap = new HashMap<String, String>();
	// folderMap.put(key, folderMap.)
	// for (Metadata root : subRoots) {
	//
	// buidTree(metadatas, root, allBackupMetadatas, newBackupMetadatas,
	// backupId);
	// }
	//
	// return newBackupMetadatas;
	// }

	private List<BackupMetadata> mergeBackupFolders(List<Metadata> metadatas, String backupId, String firstExistParentId,
			List<BackupMetadata> allBackupMetadatas, List<CountAffected> affectMetadatas, Iterable<BackupMetadata> unbackupMetadata,
			List<String> deleteUnbackup) {
		Map<String, String> mapping = new HashMap<String, String>();
		String tempPid = firstExistParentId, tempId = null, curPid = null, prevPid = backupId;
		mapping.put(tempPid, backupId);
		List<BackupMetadata> newBackupMetadatas = new LinkedList<BackupMetadata>();
		CountAffected tempAffected = getCountAffected(affectMetadatas, prevPid);
		BackupMetadata tempItem = null;
		for (Metadata tempMeta : metadatas) {

			tempPid = tempMeta.getParentId();
			tempId = StringUtils.getUUID();
			curPid = mapping.get(tempPid);

			if (StringUtils.isNullOrEmpty(curPid)) {
				curPid = getBackupRootId(tempPid, allBackupMetadatas);
				if (null == curPid) {
					throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
				}

			}

			if (!curPid.equals(prevPid)) {
				tempAffected = getCountAffected(affectMetadatas, curPid);
			}

			tempItem = TypeConvert.convert(tempMeta, tempId, curPid);
			newBackupMetadatas.add(tempItem);
			// tempItem = existUnbackup(unbackupMetadata, tempMeta);
			deleteOldUnbackup(unbackupMetadata, tempItem, tempAffected, deleteUnbackup);
			tempAffected.addFolderCount(1);
			mapping.put(tempMeta.getId(), tempId);
		}

		return newBackupMetadatas;
	}

	private void deleteOldUnbackup(Iterable<BackupMetadata> unbackups, final BaseMetadata curBackup, CountAffected tempAffected,
			List<String> deleteUnbackup) {
		Iterable<BackupMetadata> temps = Iterables.filter(unbackups, new Predicate<BackupMetadata>() {
			@Override
			public boolean apply(BackupMetadata backupItem) {
				return backupItem.getParentId().equals(curBackup.getParentId()) && backupItem.getName().equals(curBackup.getName())
						&& backupItem.getFullSourcePath().equals(curBackup.getFullSourcePath());
			}

		});
		if (null != temps) {
			for (BackupMetadata temp : temps) {
				deleteUnbackup.add(temp.getId());
				tempAffected.addFolderCount(1);
				tempAffected.addSize(-temp.getTotalSize());
			}
			
		}
	}

	public CountAffected getCountAffected(List<CountAffected> affectMetadatas, final String parentId) {

		CountAffected affected = Iterables.find(affectMetadatas, new Predicate<CountAffected>() {
			@Override
			public boolean apply(CountAffected affect) {
				return affect.getParentId().equals(parentId);
			}
		}, null);

		if (null == affected) {
			affected = new CountAffected(parentId);
			affectMetadatas.add(affected);

		}
		return affected;
	}

	public void addAffect() {

	}

	private List<BackupMetadata> mergeBackupFiles(List<Metadata> files, List<BackupMetadata> allBackupMetadatas, String backupRootId,
			String commonParentId) {
		List<BackupMetadata> newBackupfiles = new ArrayList<BackupMetadata>();
		BackupMetadata tempBackupFile = null;

		String prevId = commonParentId, prevBackupId = backupRootId, parentId = null, curId = "";
		for (Metadata file : files) {
			curId = StringUtils.getUUID();
			if (!file.getParentId().equals(prevId)) {
				parentId = getBackupRootId(file.getParentId(), allBackupMetadatas);
				if (parentId == null) {
					throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
				}
				prevId = file.getId();
				prevBackupId = curId;
			} else {
				parentId = prevBackupId;
			}

			tempBackupFile = TypeConvert.convert(file, curId, parentId);
			// allBackupMetadatas.add(tempBackupFile);
			newBackupfiles.add(tempBackupFile);
		}
		return newBackupfiles;
	}

	private void buidTree(List<Metadata> metadatas, Metadata root, List<BackupMetadata> allBackupMetadatas,
			List<BackupMetadata> newBackupMetadatas, String parentId) {

		String id = StringUtils.getUUID();
		BackupMetadata tempBackupMetadata = null;
		tempBackupMetadata = TypeConvert.convert(root, id, parentId);
		allBackupMetadatas.add(tempBackupMetadata);
		newBackupMetadatas.add(tempBackupMetadata);
		Iterable<Metadata> roots = findNextRoots(metadatas, root.getId());
		if (roots != null) {
			for (Metadata subBoot : roots) {
				buidTree(metadatas, subBoot, allBackupMetadatas, newBackupMetadatas, id);
			}

		}

	}

	private Iterable<Metadata> findNextRoots(List<Metadata> metadatas, final String id) {
		return Iterables.filter(metadatas, new Predicate<Metadata>() {

			@Override
			public boolean apply(Metadata metadata) {
				return metadata.getParentId().equals(id);
			}

		});
	}

	private List<Metadata> getNotExistsFolders(final List<BackupMetadata> oldBackupMetadatas, List<Metadata> folders) {
		// return Lists.newArrayList(Iterables.filter(folders,
		// new Predicate<Metadata>() {
		// @Override
		// public boolean apply(final Metadata folder) {
		// return !Iterables.any(oldBackupMetadatas,
		// new Predicate<BackupMetadata>() {
		//
		// @Override
		// public boolean apply(BackupMetadata backup) {
		// return backup.getOriginalIndexId()
		// .equals(folder.getId());
		// }
		// });
		// }
		//
		// }));

		Set<String> sets = new LinkedHashSet<String>(Collections2.transform(folders, new Function<Metadata, String>() {

			@Override
			public String apply(Metadata folder) {
				return folder.getId();
			}

		}));

		sets.removeAll(Collections2.transform(oldBackupMetadatas, new Function<BackupMetadata, String>() {
			@Override
			public String apply(BackupMetadata backup) {
				return backup.getOriginalIndexId();
			}
		}));
		List<Metadata> notExistsFolder = new ArrayList<Metadata>(sets.size());
		for (final String folderId : sets) {
			notExistsFolder.add(Iterables.find(folders, new Predicate<Metadata>() {

				@Override
				public boolean apply(Metadata item) {
					return item.getId().equals(folderId);
				}

			}));
		}

		return notExistsFolder;
	}

	private String getBackupRootId(final String metadataId, List<BackupMetadata> allBackupMetadatas) {
		String root = null;
		BackupMetadata tempBackupMetadata = Iterables.find(allBackupMetadatas, new Predicate<BackupMetadata>() {

			@Override
			public boolean apply(BackupMetadata backup) {
				return backup.getOriginalIndexId().equals(metadataId);
			}

		}, null);
		if (tempBackupMetadata != null) {
			root = tempBackupMetadata.getId();
		}

		return root;
	}

	private List<BackupMetadata> getBackupByOriginId(List<BackupMetadata> allBackupMetadatas, final String originId) {

		return Lists.newArrayList(Iterables.filter(allBackupMetadatas, new Predicate<BackupMetadata>() {

			@Override
			public boolean apply(BackupMetadata item) {
				return item.getOriginalIndexId().equals(originId);
			}

		}));

	}

	private List<Metadata> findRoots(List<Metadata> metadatas) {
		List<Metadata> roots = new ArrayList<Metadata>();
		for (final Metadata metadata : metadatas) {
			if (!Iterables.any(metadatas, new Predicate<Metadata>() {

				@Override
				public boolean apply(Metadata item) {
					return metadata.getParentId().equals(item.getId());
				}
			})) {

				roots.add(metadata);
			}
		}

		return roots;
	}

	/* delete un_backup file */
	private void clearFolder(String ownerId, String folderId) {
		List<BackupMetadata> unbackupMetadatas = backupMetadataDao.getUnbackupMetadatas(ownerId, folderId);
		deleteUnBackup(unbackupMetadatas);
	}

	private void recursiveClearFolder(String ownerId, String folderId) {
		clearFolder(ownerId, folderId);
		List<BackupMetadata> subFolders = backupMetadataDao.getSubFolder(folderId);
		if (!StringUtils.isNullOrEmpty(subFolders)) {
			for (BackupMetadata folder : subFolders) {
				recursiveClearFolder(ownerId, folder.getId());
			}
		}

	}

	private void deleteUnBackup(List<BackupMetadata> unbackupMetadatas) {

		if (StringUtils.isNullOrEmpty(unbackupMetadatas)) {
			return;
		}

		List<String> deleteIds = new ArrayList<String>(), tempIds = null;
		List<MetadataAttr> modifyAttrs = new ArrayList<MetadataAttr>();
		for (BackupMetadata backupMetadata : unbackupMetadatas) {
			// add self to deletes
			deleteIds.add(backupMetadata.getId());
			// add parents to modifies
			tempIds = getParentIds(backupMetadata.getId());
			if (!StringUtils.isNullOrEmpty(tempIds)) {
				for (String pid : tempIds) {
					if (backupMetadata.isFolder()) {
						// is folder only folders count minus 1
						modifyAttrs.add(new MetadataAttr(pid, true, -1, 0, 0L, 0, 0));
					} else {
						modifyAttrs.add(new MetadataAttr(pid, true, 0, -1, -backupMetadata.getSize(), backupMetadata.getWidth(),
								backupMetadata.getHeight()));
					}
				}
			}
		}

		metadataAttrService.deleteMetadataAttrs(deleteIds);
		metadataAttrService.batchUpdateMetadataAttrOnOrigin(modifyAttrs);
		backupMetadataDao.deleteBackupByIds(deleteIds);
	}

	@Override
	public List<BackupMetadata> getAllSubFoldersAndFiles(BackupMetadata backupMetadata) {
		List<BackupMetadata> resultList = new ArrayList<BackupMetadata>();
		List<BackupMetadata> checkList = new ArrayList<BackupMetadata>();
		List<BackupMetadata> tempList;
		BackupMetadata parentMetadata;
		checkList.add(backupMetadata);
		while (checkList.size() > 0) {
			parentMetadata = checkList.get(0);
			tempList = backupMetadataDao.getBackupMetadatasByParentId(parentMetadata.getId());
			resultList.addAll(tempList);
			for (int i = 0; i < tempList.size(); i++) {
				if (tempList.get(i).isFolder()) {
					checkList.add(tempList.get(i));
				}
			}
			checkList.remove(0);
		}
		return resultList;
	}

	@Override
	public void updateBackupCount(Metadata metadata, int count) {
		metadataDao.updateBackupCount(metadata.getId(), count);
		if (metadata.isFolder()) {
			List<Metadata> metadatas = metadataFactory.getAllSubFoldersAndFiles(metadata);
			for (Metadata meta : metadatas) {
				metadataDao.updateBackupCount(meta.getId(), count);
				if (!meta.isFolder()) {
					backupMetadataDao.unbackupByBackupCount(meta.getId(), count);
				}
			}
		} else {
			backupMetadataDao.unbackupByBackupCount(metadata.getId(), count);
		}
	}

	@Override
	public void unbackupMetadata(String originalIndexId) throws MetadataException {

		if (StringUtils.isNullOrEmpty(originalIndexId)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		backupMetadataDao.cancelBackupFile(originalIndexId);
	}

	@Override
	public void unbackupMetadatas(List<String> originalIndexIds) throws MetadataException {

		if (StringUtils.isNullOrEmpty(originalIndexIds)) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}

		backupMetadataDao.cancelBackupFile(originalIndexIds);
	}

}
