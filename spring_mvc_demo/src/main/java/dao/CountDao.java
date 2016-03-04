package dao;

import java.util.ArrayList;
import java.util.List;

import model.BaseMetadata;
import model.CountNode;
import model.MetadataAttr;

public interface CountDao {

	public List<CountNode> addFolders(List<CountNode> counts);

	public List<CountNode> addFiles(List<CountNode> counts);

	public int delete(List<String> idList);

	public List<CountNode> updateParent(List<CountNode> counts);

	public MetadataAttr queryCount(String id);

	public ArrayList<CountNode> checkExist(List<String> idList);

	public List<CountNode> updateParentFolder(List<CountNode> counts);

	public List<CountNode> findNotLeaf(List<String> idList);

	public List<CountNode> findNotSingleLeaf(List<String> idList);

	public List<CountNode> updateSingleLeafParents(List<CountNode> countNodes);

	public List<CountNode> updateNotSingleLeafParents(List<CountNode> countNodes);

	public List<String> updateNodeFolder(List<String> idList);

	public BaseMetadata getNode(String id);

	public BaseMetadata getBackUpNode(String id);

	public BaseMetadata getNodeByOriginId(String id);

}
