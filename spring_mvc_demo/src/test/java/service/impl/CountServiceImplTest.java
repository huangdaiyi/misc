package service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import model.BaseMetadata;
import model.CountAffected;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import service.CountService;
import base.BaseTestCase;
import dao.CountDao;

public class CountServiceImplTest extends BaseTestCase {
	@Autowired
	CountService countService;

	@Autowired
	CountDao countDao;

	@Mock
	private CountDao baseMetaDaoMock;

	@Mock
	private CountService baseMetaServiceMock;

	@Mock
	private CountServiceImpl baseMetaServiceImpMock;

	@Before
	public void initMocks() {
		MockitoAnnotations.initMocks(this);
	}

	/**
	 *
	 */

	public void AddFolders() {
		List<BaseMetadata> lst = new ArrayList<BaseMetadata>();
		List<String> parentId1 = new ArrayList<String>();
		List<String> parentId2 = new ArrayList<String>();
		List<String> parentId3 = new ArrayList<String>();
		List<String> parentId4 = new ArrayList<String>();
		List<String> parentId5 = new ArrayList<String>();
		List<String> parentId6 = new ArrayList<String>();
		List<String> parentId7 = new ArrayList<String>();

		parentId1.add("0");
		parentId2.add("96c29b1d-d626-4cb6-8665-8dff75c58121");
		parentId3.add("96c29b1d-d626-4cb6-8665-8dff75c58121");
		parentId3.add("96c29b1d-d626-4cb6-8665-8dff75c58122");
		parentId4.add("96c29b1d-d626-4cb6-8665-8dff75c58121");
		parentId4.add("96c29b1d-d626-4cb6-8665-8dff75c58122");
		parentId4.add("96c29b1d-d626-4cb6-8665-8dff75c58123");
		parentId5.add("96c29b1d-d626-4cb6-8665-8dff75c58121");
		parentId5.add("96c29b1d-d626-4cb6-8665-8dff75c58122");
		parentId6.add("96c29b1d-d626-4cb6-8665-8dff75c58121");
		parentId6.add("96c29b1d-d626-4cb6-8665-8dff75c58122");
		parentId6.add("96c29b1d-d626-4cb6-8665-8dff75c58125");
		parentId7.add("96c29b1d-d626-4cb6-8665-8dff75c58121");
		parentId7.add("96c29b1d-d626-4cb6-8665-8dff75c58122");
		parentId7.add("96c29b1d-d626-4cb6-8665-8dff75c58125");

		BaseMetadata up1 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58121",
				false, true);

		BaseMetadata up2 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58122",
				false, false);

		BaseMetadata up3 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58123",
				false, false);

		BaseMetadata up4 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58124",
				false, false);

		BaseMetadata up5 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58125",
				false, false);

		BaseMetadata up6 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58126",
				false, false);

		BaseMetadata up7 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58127",
				false, false);

		lst.add(up1);
		lst.add(up2);
		lst.add(up3);
		lst.add(up4);
		lst.add(up5);
		lst.add(up6);
		lst.add(up7);
		List<BaseMetadata> rst_mock = new ArrayList<BaseMetadata>();
		// Mockito.when(baseMetaServiceImpMock.addFolders(lst)).thenReturn(lst);
		countService.recalculateForCreatingFolders(lst);
	}

	public void AddFiles() {
		List<BaseMetadata> lst = new ArrayList<BaseMetadata>();
		BaseMetadata ct1 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58111",
				true, true);
		ct1.setParentId("96c29b1d-d626-4cb6-8665-8dff75c58124");
		BaseMetadata ct2 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58112",
				true, false);
		ct2.setParentId("96c29b1d-d626-4cb6-8665-8dff75c58125");
		lst.add(ct1);
		lst.add(ct2);
		countService.recalculateForCreatingFiles(lst);
	}

	public void DeleteFolder() {
		List<BaseMetadata> lst = new ArrayList<BaseMetadata>();
		BaseMetadata up4 = createBase("96c29b1d-d626-4cb6-8665-8dff75c58122",
				false, false);
		lst.add(up4);
		countService.recalculateForDeletingFolders(lst);
	}

	public void DeleteFiles() {
		List<BaseMetadata> lst = new ArrayList<BaseMetadata>();
		BaseMetadata ct_pre = createBase(
				"96c29b1d-d626-4cb6-8665-8dff75c58112", true, false);
		lst.add(ct_pre);
		countService.recalculateForDeletingFiles(lst);
	}

	private ArrayList<String> getParentIdList() {
		ArrayList<String> parentId = new ArrayList<String>();
		parentId.add("96c29b1d-d626-4cb6-8665-8dff75c58121");
		parentId.add("96c29b1d-d626-4cb6-8665-8dff75c58122");
		return parentId;
	}

	private BaseMetadata createBase(String id, boolean isFile, boolean isBackUp) {
		if (id == null || id.equals("")) {
			id = UUID.randomUUID().toString();
		}
		BaseMetadata baseMeta = new BaseMetadata();
		baseMeta.setId(id);
		baseMeta.setBackup(true);

		if (isFile) {
			baseMeta.setSize(1000L);
		} else {
			baseMeta.setSize(0);
		}
		return baseMeta;
	}

	private void testCount() {
		List<CountAffected> list = new ArrayList<CountAffected>();
	}

	@Test
	public void testAll() {
		// resetCount();
		// AddFolders();
		// AddFiles();
		// DeleteFiles();
		// DeleteFolder();
	}
}
