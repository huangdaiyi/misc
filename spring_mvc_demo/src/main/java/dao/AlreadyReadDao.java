package dao;

import java.util.List;

import model.AlreadyRead;

public interface AlreadyReadDao {

	public void updateAlreadyRead(String metadataIndexId, String userId);

	public List<AlreadyRead> getAlreadyRead(String metadataIndexId);

	public List<AlreadyRead> batchGetAlreadyRead(List<String> metadataIndexIds);

	public boolean isAlreadyRead(String userId, String metadataIndexId);
}