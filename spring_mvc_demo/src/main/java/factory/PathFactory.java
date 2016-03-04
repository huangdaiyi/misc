package factory;

import model.PathInfo;
import model.PathRequestBase;
import constants.CommonFolders;

public interface PathFactory {

	public CommonFolders mappingToCommonFolderByExtension(String sourcePath);

	public PathInfo parsePathInfo(PathRequestBase pathRequestBase, boolean isFolder, boolean isPathExist);

	public String fetchDeviceUid(String sourcePath);

	public boolean isStreamingFile(String name);

	public boolean isReaderFile(String name);

	public boolean isPicture(String name);

	public void checkCanCreateFile(String path);

	public void checkCanMoveFileFrom(String path);

	public void checkCanMoveCopyFileTo(String path);

	public void checkCanCreateFolder(String path);

	public void checkCanCreateCollaborate(String path);

	public void checkCanNoteFolder(String path);

	public void checkCanMoveFolderFrom(String path);

	public void checkCanMoveFolderTo(String path);

	public void checkCanMoveFolderToBeOverride(String toBePath);

	public void checkCanCopyFolderFrom(String path);

	public void checkCanCopyFolderTo(String path);

	public void checkCanCopyFolderToBeOverride(String toBePath);

	public void checkCanLinkFolder(String path);

	public void checkCanUnlinkFolder(String path);

	public void checkCanZipUnzip(String path);

}