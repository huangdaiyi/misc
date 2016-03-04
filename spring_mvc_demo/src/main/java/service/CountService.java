package service;

import java.util.List;

import model.BaseMetadata;
import model.CountAffected;
import model.CountManageModel;
import model.CountResult;
import model.request.CountRequest;

public interface CountService {

	public void recalculateForDeletingFolders(
			List<? extends BaseMetadata> foldersToDelete);

	public void recalculateForCreatingFiles(
			List<? extends BaseMetadata> filesToCreate);
	
	public void recalculateForUpdatingFiles(
			List<? extends BaseMetadata> filesToUpdate);

	public void recalculateForDeletingFiles(
			List<? extends BaseMetadata> filesToDelete);

	public void recalculateForCreatingFolders(
			List<? extends BaseMetadata> foldersToCreate);

	public CountResult getSubCount(CountRequest request);

	public void updateAllParentCount(List<CountAffected> list);

	public void updateAllParentCountBackup(List<CountAffected> list);

	public void updateAllParentCount(CountManageModel model);

	public void updateAllParentCountBackup(CountManageModel model);
	

}
