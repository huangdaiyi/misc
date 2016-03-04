package dao;

import java.util.List;

import model.ReaderFileMetadata;
import model.ReaderPageDetail;
import model.SourceTargetMap;

public interface ReaderFileMetadataDao {

	public void copyFileProcessingStatus(String sourceId, String targetId);

	public void copyReaderFileMetadata(String sourceId, String targetId);

	public void copyReaderFileMetadataDetail(String sourceId, String targetId);

	public ReaderFileMetadata getReaderFileMetadata(String metadataIndexId);

	public List<ReaderPageDetail> getReaderPageDetails(String metadataIndexId);

	public ReaderFileMetadata replaceReaderFileMetadata(ReaderFileMetadata readerFileMetadata);

	public List<ReaderPageDetail> replaceReaderPageDetail(String metadataIndexId, List<ReaderPageDetail> readerPageDetails);

	public void deleteReaderFileMetadataAndPageDetails(String metadataIndexId);

	public void copyReaderFileMetadataDetail(List<SourceTargetMap> maps);

	public void copyReaderFileMetadata(List<SourceTargetMap> maps);

	public void copyFileProcessingStatus(List<SourceTargetMap> maps);

	public ReaderFileMetadata getReaderBackupFileMetadata(String metadataIndexId);

}
