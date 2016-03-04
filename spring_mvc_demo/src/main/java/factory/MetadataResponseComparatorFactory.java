package factory;

import java.util.Comparator;

import model.response.FileResponse;
import model.response.FolderResponse;
import model.response.GetCollaborateInfoResponse;
import constants.SortType;

public interface MetadataResponseComparatorFactory {

	public Comparator<FileResponse> generateFileResponseComparator(SortType sortType);

	public Comparator<FolderResponse> generateFolderResponseComparator(SortType sortType);
	
	public Comparator<GetCollaborateInfoResponse> generateCollaborateInfoResponseComparator(SortType sortType);

}
