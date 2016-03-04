package service.impl;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import model.BackupMetadata;
import model.BaseMetadata;
import model.CountAffected;
import model.CountManageModel;
import model.CountNode;
import model.CountResult;
import model.Metadata;
import model.MetadataAttr;
import model.request.CountRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import service.CountService;
import utils.StringUtils;
import constants.EOperateType;
import constants.HttpStatus;
import dao.BackupMetadataDao;
import dao.CountDao;
import dao.MetadataAttrDao;
import dao.MetadataDao;
import dao.UserDao;
import exception.MetadataException;

@Service
public class CountServiceImpl implements CountService {

	@Autowired
	private CountDao countDao;
	@Autowired
	private UserDao userDao;
	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private BackupMetadataDao backupMetadataDao;
	@Autowired
	private MetadataAttrDao metadataAttrDao;
	@Autowired
	FolderServiceImpl folderServiceImpl;

	private List<CountNode> addFolders(List<CountNode> countNodes) {
		synchronized (this) {
			List<String> idList = buildIdString(countNodes);
			List<CountNode> lst = countDao.checkExist(idList);// query
			List<CountNode> newFolders = selectWhetherExist(countNodes, lst,
					false);
			List<CountNode> result = null;
			if (newFolders.size() > 0) {
				countDao.addFolders(newFolders); // add
				for (CountNode item : newFolders) {
					List<String> parentIds = item.getAllParentId();// query
					item.setAllParentId(parentIds);
				}
				result = countDao
						.updateNotSingleLeafParents(buildParent(countNodes));// update
			}
			return result;
		}
	}

	private List<CountNode> addFiles(List<CountNode> counts) {
		synchronized (this) {
			countDao.addFiles(counts);
			for (CountNode node : counts) {
				node.setLeafFileCount(1);
				node.setLeafFolderCount(0);
			}
			List<CountNode> rst = buildParent(counts);
			return countDao.updateParent(rst);// query
		}
	}

	private void updateFiles(List<CountNode> counts) {
		synchronized (this) {
			for (CountNode node : counts) {
				MetadataAttr attr = countDao.queryCount(node.getLeafId());
				node.setLeafFileCount(0);
				node.setLeafFolderCount(0);
				node.setLeafSize(node.getLeafSize() - attr.getTotalSize());
			}
			List<CountNode> rst = buildParent(counts);
			rst.addAll(counts);
			countDao.updateParent(rst);// query
		}
	}

	private List<CountNode> deleteFolders(List<CountNode> fordelete) {
		synchronized (this) {
			if (fordelete != null && fordelete.size() > 0) {
				fordelete = ensureNegative(fordelete);
				List<CountNode> parentNode = new ArrayList<CountNode>();
				for (CountNode item : fordelete) {
					for (String pId : item.getAllParentId()) {
						CountNode node = new CountNode();
						node.setLeafId(pId);
						node.setLeafFileCount(item.getLeafFileCount());
						node.setLeafFolderCount(item.getLeafFolderCount() == 0 ? -1
								: item.getLeafFolderCount() - 1);
						node.setLeafSize(item.getLeafSize());
						parentNode.add(node);
					}
				}
				countDao.updateParent(parentNode);// update
				List<String> fatherId = new ArrayList<String>();
				List<String> allParentId = new ArrayList<String>();
				for (CountNode item : fordelete) {
					fatherId.add(item.getFatherId());
					for (String id : item.getAllParentId()) {
						allParentId.add(id);
					}
				}
				allParentId.removeAll(fatherId);
				if (allParentId.size() > 0) {
					countDao.updateNodeFolder(allParentId);
				}
			}
			countDao.delete(buildIdString(fordelete));// delete
			return fordelete;
		}
	}

	private boolean deleteFiles(List<CountNode> counts) {
		synchronized (this) {
			List<CountNode> negativeCounts = ensureNegative(counts);
			List<CountNode> lst = buildParent(negativeCounts);
			countDao.updateParent(lst);
			int row = countDao.delete(buildIdString(negativeCounts));
			return row > 0;
		}
	}

	private List<CountNode> buildParent(List<CountNode> counts) {
		List<CountNode> lst = new ArrayList<CountNode>();
		for (CountNode item : counts) {
			if (item.getAllParentId() != null
					&& item.getAllParentId().size() > 0) {
				for (String id : item.getAllParentId()) {
					CountNode ct = new CountNode();
					ct.setLeafId(id);
					ct.setLeafSize(item.getLeafSize());
					ct.setLeafFileCount(item.getLeafFileCount());
					ct.setLeafFolderCount(item.getLeafFolderCount());
					lst.add(ct);
				}
			}
		}
		return lst;
	}

	private List<CountNode> selectWhetherExist(List<CountNode> all,
			List<CountNode> part, boolean exist) {
		List<CountNode> rst = new ArrayList<CountNode>();
		if (part == null || part.size() == 0) {
			return all;
		}
		if (exist) {
			for (CountNode p : part) {
				for (CountNode a : all) {
					if (p.getLeafId().equals(a.getLeafId())) {
						rst.add(a);
					}
				}
			}
		} else {
			for (CountNode a : all) {
				boolean flag = false;
				if (part != null && part.size() > 0) {
					for (CountNode p : part) {
						if (p.getLeafId().equals(a.getLeafId())) {
							flag = true;
						}
					}
					if (!flag) {
						rst.add(a);
					}
				} else {
					return all;
				}
			}
		}

		return rst;
	}

	private List<String> buildIdString(List<CountNode> counts) {
		List<String> lst = new ArrayList<String>();
		for (CountNode count : counts) {
			lst.add(count.getLeafId());
		}
		return lst;
	}

	private List<CountNode> ensureNegative(List<CountNode> counts) {
		for (CountNode item : counts) {
			if (item.getLeafFolderCount() > 0) {
				item.setLeafFolderCount(-1 * item.getLeafFolderCount());
			}
			if (!item.isFolder()) {// when delete a file,for parentNode
				item.setLeafFileCount(-1);
			}
			if (item.getLeafSize() > 0) {
				item.setLeafSize(-1 * item.getLeafSize());
			}
		}
		return counts;
	}

	private List<String> getAllParentId(String leafid, boolean isbackUp) {
		List<String> rst = new ArrayList<String>();
		BaseMetadata metadata = null;
		if (isbackUp) {
			metadata = countDao.getNodeByOriginId(leafid);
		} else {
			metadata = countDao.getNode(leafid);
		}
		Queue<String> queue = new LinkedList<String>();
		queue.offer(metadata.getParentId());
		while (queue.size() > 0) {
			String tempId = queue.poll();
			if (!StringUtils.isNullOrEmpty(tempId)) {
				BaseMetadata tempNode = null;
				if (isbackUp) {
					tempNode = countDao.getBackUpNode(tempId);
				} else {
					tempNode = countDao.getNode(tempId);
				}
				queue.offer(tempNode.getParentId());
				rst.add(tempNode.getId());
			}
		}
		return rst;
	}

	@SuppressWarnings("static-access")
	private List<CountNode> adapter(List<? extends BaseMetadata> folderOrfile,
			EOperateType type) {
		boolean wait = true;
		Long startTime = System.currentTimeMillis();
		while (wait) {
			try {
				BaseMetadata testEntity = folderOrfile.get(0);
				wait = testEntity == null ? false : true;
				if (wait) {
					List<String> parentIds = getAllParentId(testEntity.getId(),
							false);
					List<String> back_parentIds = getAllParentId(
							testEntity.getId(), true);
					if (parentIds.size() * back_parentIds.size() > 0) {
						wait = false;
					} else {
						Thread.currentThread().sleep(2000);// 2s interval
					}
					Long endTime = System.currentTimeMillis();
					if (endTime - startTime > 240000) {// 4 min timeOut
						wait = false;
					}
				}
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}

		List<CountNode> countNodes = new ArrayList<CountNode>();
		List<String> originIds = new ArrayList<String>();
		for (BaseMetadata item : folderOrfile) {
			originIds.add(item.getId());
		}
		List<BackupMetadata> list_backUp = backupMetadataDao
				.getBackups(originIds);
		List<BaseMetadata> list_all = new ArrayList<BaseMetadata>();
		for (BackupMetadata item : list_backUp) {
			BaseMetadata metadata = new BaseMetadata();
			metadata.setId(item.getId());
			metadata.setFilesCount(item.getFilesCount());
			metadata.setBackup(true);
			metadata.setFolder(item.isFolder());
			metadata.setTotalSize(item.getSize());
			metadata.setParentId(item.getParentId());
			metadata.setOriginId(item.getOriginalIndexId());
			list_all.add(metadata);
		}
		for (BaseMetadata item : folderOrfile) {
			list_all.add(item);
		}
		if (!EOperateType.ADD_FOLDER.equals(type)) {
			for (BaseMetadata item : list_all) {

				List<String> parentIds = new ArrayList<String>();
				if (item.isBackup()) {
					parentIds = getAllParentId(item.getOriginId(),
							item.isBackup());
				} else {
					parentIds = getAllParentId(item.getId(), item.isBackup());
				}
				// query
				CountNode count = new CountNode();
				if (type.equals(EOperateType.DELETE)) {
					MetadataAttr metadataAttr = countDao.queryCount(item// query
							.getId());
					count.setLeafFileCount(metadataAttr.getFilesCount());
					count.setLeafFolderCount(metadataAttr.getFoldersCount());
					count.setLeafSize(metadataAttr.getTotalSize());
				} else if (type.equals(EOperateType.UPDATE_FILE)) {
					count.setLeafSize(item.getTotalSize());
				} else if (type.equals(EOperateType.ADD)) {
					count.setLeafSize(item.getTotalSize());
					if (!item.isFolder()) {
						count.setLeafFileCount(1);
					} else {
						count.setLeafFileCount(0);
					}
				}
				count.setAllParentId(parentIds);
				count.setBackup(item.isBackup());
				count.setFatherId(item.getParentId());
				count.setFolder(item.isFolder());
				count.setLeafId(item.getId());
				countNodes.add(count);
			}
		} else {
			for (BaseMetadata item : list_all) {
				List<String> parentIds = null;
				if (item.isBackup()) {
					parentIds = getAllParentId(item.getOriginId(),
							item.isBackup());// query
				} else {
					parentIds = getAllParentId(item.getId(), item.isBackup());
				}
				CountNode count = new CountNode();
				count.setAllParentId(parentIds);
				count.setBackup(item.isBackup());
				count.setFatherId(item.getParentId());
				count.setFolder(item.isFolder());
				count.setLeafFileCount(0);
				count.setLeafFolderCount(1);
				count.setLeafId(item.getId());
				count.setLeafSize(0L);
				countNodes.add(count);
			}
		}
		return countNodes;
	}

	@Override
	@Transactional
	public void recalculateForDeletingFolders(
			final List<? extends BaseMetadata> foldersToDelete) {
		Thread thread = new Thread() {
			public void run() {
				List<CountNode> counts = adapter(foldersToDelete,
						EOperateType.DELETE);
				deleteFolders(counts);
			}
		};
		thread.setDaemon(true);
		thread.start();
	}

	@Override
	@Transactional
	public void recalculateForCreatingFiles(
			final List<? extends BaseMetadata> filesToCreate) {
		Thread thread = new Thread() {
			public void run() {
				List<CountNode> counts = adapter(filesToCreate,
						EOperateType.ADD);
				addFiles(counts);
			}
		};
		thread.setDaemon(true);
		thread.start();
	}

	@Override
	@Transactional
	public void recalculateForUpdatingFiles(
			final List<? extends BaseMetadata> filesToUpdate) {
		// Thread thread = new Thread() {
		// public void run() {
		List<CountNode> counts = adapter(filesToUpdate,
				EOperateType.UPDATE_FILE);
		updateFiles(counts);
		// }
		// };
		// thread.setDaemon(true);
		// thread.start();
	}

	@Override
	@Transactional
	public void recalculateForDeletingFiles(
			final List<? extends BaseMetadata> filesToDelete) {
		Thread thread = new Thread() {
			public void run() {
				List<CountNode> counts = adapter(filesToDelete,
						EOperateType.DELETE);
				deleteFiles(counts);
			}
		};
		thread.setDaemon(true);
		thread.start();
	}

	@Override
	public void recalculateForCreatingFolders(
			final List<? extends BaseMetadata> foldersToCreate) {
		Thread thread = new Thread() {
			public void run() {
				List<CountNode> counts = adapter(foldersToCreate,
						EOperateType.ADD_FOLDER);
				addFolders(counts);
			}
		};
		thread.setDaemon(true);
		thread.start();
	}

	public void updateRoot(MetadataAttr metadataAttr) {
		metadataAttrDao.updateMetadataAttr(metadataAttr);
	}

	@Override
	public CountResult getSubCount(CountRequest request) {
		String ownerId = StringUtils.isNullOrEmpty(request.getOwnerId()) ? request
				.getUserId() : request.getOwnerId();
		String fullPath = request.getPath();
		boolean isBackUp = request.isBackUp();
		fullPath = ownerId.concat("/").concat(fullPath);
		String[] paths = fullPath.split("/");
		Queue<String> queueID = new LinkedList<String>();
		CountResult countResult = new CountResult();
		queueID.offer("");
		for (String p : paths) {// get the position
			List<String> parent_Ids = new ArrayList<String>();
			List<String> parent_Names = new ArrayList<String>();
			parent_Names.add(p);
			while (queueID.size() > 0) {
				String tempId = queueID.poll();
				parent_Ids.add(tempId);
			}
			if (!StringUtils.isNullOrEmpty(p)) {
				List<Metadata> rst = null;
				rst = userDao.getSubLevel(ownerId, parent_Ids, parent_Names,
						isBackUp);
				if (rst.size() > 0) {
					Metadata metadata = rst.get(0);
					queueID.offer(metadata.getId());
				} else {
					throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
				}
			}
		}
		while (queueID.size() > 0) {
			List<String> parentId_list = new ArrayList<String>(20);
			Hashtable<String, String> tempTable = new Hashtable<String, String>();
			while (queueID.size() > 0) {
				parentId_list.add(queueID.poll());
			}
			List<Metadata> temp = userDao.getSubLevel(ownerId, parentId_list,
					null, isBackUp);
			for (Metadata metadata : temp) {
				if (metadata.isFolder()) {
					// forFatherLevel.add(metadata);
					tempTable.put(metadata.getParentId(), "");
					queueID.offer(metadata.getId());
					countResult
							.setFolderCount(countResult.getFolderCount() + 1);
				} else {
					countResult.setFileSize(countResult.getFileSize()
							+ metadata.getSize());
					countResult.setFileCount(countResult.getFileCount() + 1);
				}
			}
			// fatherLevel = forFatherLevel;
		}
		return countResult;
	}

	// ****************************************************************************2015.6.29-tristan

	@Override
	public void updateAllParentCount(CountManageModel model) {
		updateALLParentNode(model.getAffected(), false);
	}

	@Override
	public void updateAllParentCountBackup(CountManageModel model) {
		updateALLParentNode(model.getAffected(), true);
	}

	@Override
	public void updateAllParentCount(List<CountAffected> list) {
		updateALLParentNode(list, false);
	}

	@Override
	public void updateAllParentCountBackup(List<CountAffected> list) {
		updateALLParentNode(list, true);
	}

	// ****************************************************************************
	private void updateALLParentNode(List<CountAffected> list, boolean isBackup) {
		Hashtable<String, String> parentIdMap = new Hashtable<String, String>();
		List<BaseMetadata> needUpdate = new ArrayList<BaseMetadata>();

		for (CountAffected item : list) {
			String id = item.getParentId();
			List<String> historyUpperIds = findAllParentId(parentIdMap, id);

			if (historyUpperIds.size() > 1) {
				List<BaseMetadata> metadatas = generateMetadata(
						historyUpperIds, item);
				needUpdate.addAll(metadatas);
			} else {
				List<String> parentIdList = new ArrayList<String>();
				parentIdList.add(id);

				if (isBackup) {
					List<String> parentIds = backupMetadataDao
							.getAllUpperIds(id);
					parentIdList.addAll(parentIds);
				} else {
					while (!StringUtils.isNullOrEmpty(id)) {
						String parentId = "";
						Metadata metadata = metadataDao.getMetadata(id);
						if (metadata != null
								&& !StringUtils.isNullOrEmpty(metadata
										.getParentId())) {
							parentId = metadata.getParentId();
						}
						if (!StringUtils.isNullOrEmpty(parentId)) {
							parentIdMap.put(id, parentId);
							parentIdList.add(parentId);
						}
						id = parentId;
					}
					List<BaseMetadata> metadatas = generateMetadata(
							parentIdList, item);
					needUpdate.addAll(metadatas);
				}
			}
		}
		metadataAttrDao.batchUpdateSizeCount(needUpdate);
	}

	// include-self
	private List<String> findAllParentId(Hashtable<String, String> table,
			String id) {
		List<String> list = new ArrayList<String>();
		list.add(id);
		while (!StringUtils.isNullOrEmpty(id)) {
			id = table.get(id);
			if (!StringUtils.isNullOrEmpty(id)) {
				list.add(id);
			}
		}
		return list;
	}

	private List<BaseMetadata> generateMetadata(List<String> list,
			CountAffected item) {
		List<BaseMetadata> result = new ArrayList<BaseMetadata>();
		for (String id : list) {
			BaseMetadata metadata = item.toBaseMetadata();
			metadata.setId(id);
			result.add(metadata);
		}
		return result;
	}

}
