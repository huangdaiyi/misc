package factory.impl;

import java.util.Comparator;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import constants.CommonFolders;
import constants.SortType;
import factory.MetadataResponseComparatorFactory;
import factory.PathFactory;
import model.response.FileResponse;
import model.response.FolderResponse;
import model.response.GetCollaborateInfoResponse;

@Component
public class MetadataResponseComparatorFactoryImpl implements MetadataResponseComparatorFactory {

	@Autowired
	private PathFactory pathFactory;

	/*
	 * commons
	 */
	private Comparator<String> stringComparator = new Comparator<String>() {
		@Override
		public int compare(String o1, String o2) {
			if (o1 == null && o2 == null) {
				return 0;
			} else if (o1 == null) {
				return -1;
			} else if (o2 == null) {
				return 1;
			} else if (o1.equals(o2)) {
				return 0;
			}
			for (int i = 0, checkLength = Math.min(o1.length(), o2.length()); i < checkLength; i++) {
				int comparedCharNumber1 = toComparedCharNumber(o1.charAt(i));
				int comparedCharNumber2 = toComparedCharNumber(o2.charAt(i));
				if (comparedCharNumber1 != comparedCharNumber2) {
					return comparedCharNumber1 - comparedCharNumber2;
				}
			}
			return o1.length() - o2.length();
		}
	};
	private Comparator<String> pathComparator = new Comparator<String>() {
		@Override
		public int compare(String o1, String o2) {
			String dirName1 = FilenameUtils.getFullPathNoEndSeparator(o1);
			String dirName2 = FilenameUtils.getFullPathNoEndSeparator(o1);
			int result = stringComparator.compare(dirName1, dirName2);
			if (result != 0) {
				return result;
			}
			String baseName1 = FilenameUtils.getName(o1);
			String baseName2 = FilenameUtils.getName(o2);
			return stringComparator.compare(baseName1, baseName2);
		}
	};

	/*
	 * file comparators
	 */
	private Comparator<FileResponse> imageFileComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			return Boolean.compare(pathFactory.isPicture(o1.getName()), pathFactory.isPicture(o2.getName()));
		}
	};
	private Comparator<FileResponse> defaultFileComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = imageFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return Boolean.compare(o1.isIndependent(), o2.isIndependent());
		}
	};
	private Comparator<FileResponse> fileCreatedAtComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return Long.compare(o2.getCreatedAt(), o1.getCreatedAt());
		}
	};
	private Comparator<FileResponse> fileNameComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return stringComparator.compare(o1.getName(), o2.getName());
		}
	};
	private Comparator<FileResponse> fileSizeComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			result = Long.compare(o1.getTotalSize(), o2.getTotalSize());
			if (result != 0) {
				return result;
			}
			return stringComparator.compare(o1.getName(), o2.getName());
		}
	};
	private Comparator<FileResponse> fileTypeComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			} else if (o1.getType() == o2.getType()) {
				return stringComparator.compare(o1.getName(), o2.getName());
			}
			return o1.getType().compareTo(o2.getType());
		}
	};
	private Comparator<FileResponse> fileModifiedAtComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return Long.compare(o2.getModifiedAt(), o1.getModifiedAt());
		}
	};
	private Comparator<FileResponse> fileUserArrangeComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return Integer.compare(o1.getSortPriority(), o2.getSortPriority());
		}
	};
	private Comparator<FileResponse> fileDeviceNameComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			String deviceName1 = (o1.getDeviceName() != null ? o1.getDeviceName() : "");
			String deviceName2 = (o2.getDeviceName() != null ? o2.getDeviceName() : "");
			if (deviceName1.equals(deviceName2)) {
				return stringComparator.compare(o1.getName(), o2.getName());
			}
			return stringComparator.compare(deviceName1, deviceName2);
		}
	};
	private Comparator<FileResponse> fileSourcePathComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return pathComparator.compare(o1.getSourcePath(), o2.getSourcePath());
		}
	};
	private Comparator<FileResponse> filePathComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return pathComparator.compare(o1.getPath(), o2.getPath());
		}
	};
	private Comparator<FileResponse> fileExtensionComparator = new Comparator<FileResponse>() {
		@Override
		public int compare(FileResponse o1, FileResponse o2) {
			int result = defaultFileComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			String extension1 = FilenameUtils.getExtension(o1.getName());
			String extension2 = FilenameUtils.getExtension(o2.getName());
			if (extension1.equalsIgnoreCase(extension2)) {
				return stringComparator.compare(o1.getName(), o2.getName());
			}
			return stringComparator.compare(extension1, extension2);
		}
	};

	/*
	 * folder comparators
	 */
	private Comparator<FolderResponse> systemFolderComparator = new Comparator<FolderResponse>() {
		@Override
		public int compare(FolderResponse o1, FolderResponse o2) {
			CommonFolders[] commonFolders = CommonFolders.values();
			int indexO1 = ArrayUtils.indexOf(commonFolders, CommonFolders.parse(o1.getPath(),o1.getPath().toLowerCase().startsWith(CommonFolders.MY_BACKUP_DATA.toString())));
			int indexO2 = ArrayUtils.indexOf(commonFolders, CommonFolders.parse(o2.getPath(),o1.getPath().toLowerCase().startsWith(CommonFolders.MY_BACKUP_DATA.toString())));
			if (indexO1 < 0 && indexO2 >= 0) {
				return 1;
			} else if (indexO1 >= 0 && indexO2 < 0) {
				return -1;
			}
			return Integer.compare(indexO1, indexO2);
		}
	};
	
	private Comparator<FolderResponse> defaultFolderComparator = new Comparator<FolderResponse>() {
		@Override
		public int compare(FolderResponse o1, FolderResponse o2) {
			return Boolean.compare(o1.isIndependent(), o2.isIndependent());
		}
	};
	
	private Comparator<FolderResponse> folderCreatedAtComparator = new Comparator<FolderResponse>() {
		@Override
		public int compare(FolderResponse o1, FolderResponse o2) {
			int result = defaultFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			result = systemFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return Long.compare(o2.getCreatedAt(), o1.getCreatedAt());
		}
	};
	private Comparator<FolderResponse> folderNameComparator = new Comparator<FolderResponse>() {
		@Override
		public int compare(FolderResponse o1, FolderResponse o2) {
			int result = defaultFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			result = systemFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return stringComparator.compare(o1.getName(), o2.getName());
		}
	};
	private Comparator<FolderResponse> folderSizeComparator = new Comparator<FolderResponse>() {
		@Override
		public int compare(FolderResponse o1, FolderResponse o2) {
			int result = defaultFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			result = systemFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return Long.compare(o1.getTotalSize(), o1.getTotalSize());
		}
	};
	private Comparator<FolderResponse> folderTypeComparator = new Comparator<FolderResponse>() {
		@Override
		public int compare(FolderResponse o1, FolderResponse o2) {
			int result = defaultFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			result = systemFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return o1.getType().compareTo(o2.getType());
		}
	};
	private Comparator<FolderResponse> folderModifiedAtComparator = new Comparator<FolderResponse>() {
		@Override
		public int compare(FolderResponse o1, FolderResponse o2) {
			int result = defaultFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			result = systemFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return Long.compare(o2.getModifiedAt(), o1.getModifiedAt());
		}
	};
	private Comparator<FolderResponse> folderUserArrangeComparator = new Comparator<FolderResponse>() {
		@Override
		public int compare(FolderResponse o1, FolderResponse o2) {
			int result = defaultFolderComparator.compare(o1, o2);
			if (result != 0) {
				return result;
			}
			return Integer.compare(o1.getSortPriority(), o2.getSortPriority());
		}
	};

	/*
	 * collaborate comparators
	 */
	private Comparator<GetCollaborateInfoResponse> collaborateInfoNameComparator = new Comparator<GetCollaborateInfoResponse>() {
		@Override
		public int compare(GetCollaborateInfoResponse o1, GetCollaborateInfoResponse o2) {
			return stringComparator.compare(o1.getName(), o2.getName());
		}
	};

	@Override
	public Comparator<FileResponse> generateFileResponseComparator(SortType sortType) {
		switch (sortType) {
		case NAME:
			return fileNameComparator;
		case SIZE:
			return fileSizeComparator;
		case TYPE:
			return fileTypeComparator;
		case MODIFIED_AT:
			return fileModifiedAtComparator;
		case USER_ARRANGE:
			return fileUserArrangeComparator;
		case DEVICE_NAME:
			return fileDeviceNameComparator;
		case SOURCE_PATH:
			return fileSourcePathComparator;
		case EXTENSION:
			return fileExtensionComparator;
		case PATH:
			return filePathComparator;
		default:
			return fileCreatedAtComparator;
		}
	}

	@Override
	public Comparator<FolderResponse> generateFolderResponseComparator(SortType sortType) {
		switch (sortType) {
		case NAME:
		case DEVICE_NAME:
		case SOURCE_PATH:
		case EXTENSION:
			return folderNameComparator;
		case SIZE:
			return folderSizeComparator;
		case TYPE:
			return folderTypeComparator;
		case USER_ARRANGE:
			return folderUserArrangeComparator;
		case MODIFIED_AT:
			return folderModifiedAtComparator;
		default:
			return folderCreatedAtComparator;
		}
	}

	@Override
	public Comparator<GetCollaborateInfoResponse> generateCollaborateInfoResponseComparator(SortType sortType) {
		return collaborateInfoNameComparator;
	}

	private int toComparedCharNumber(char c) {
		if (c < 48 || c > 126) {
			return c;
		}
		int range123 = 10, rangeCode1 = 65 - 58, rangeABC = 26, rangeCode2 = 97 - 91, rangeAbc = 26, rangeCode3 = 127 - 123;

		if (c <= 126 && c > 122) {
			return c - range123 - rangeAbc - rangeABC;
		} else if (c <= 122 && c > 96) {
			return c + rangeCode3;
		} else if (c <= 96 && c > 90) {
			return c - range123 - rangeAbc;
		} else if (c <= 90 && c > 64) {
			return c + rangeCode3 + rangeCode2;
		} else if (c <= 64 && c > 57) {
			return c - range123;
		} else if (c <= 57 && c > 47) {
			return c + rangeCode3 + rangeCode2 + rangeCode1;
		}
		return c;
	}

}
