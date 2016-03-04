package service;

import java.util.List;

import model.ActiveHistory;
import model.CellPhone;
import model.SSOToken;

public interface DynamoDBService extends UserAuthorizationService {

	public SSOToken getSSOToken(String token);

	public List<CellPhone> findCellphonesByUserId(String userId);

	public void createActivityHistory(List<ActiveHistory> historys);
	
	public void createActivityHistory(String fullPath, String oldFullPath,
			String sharedUpperPath, String oldSharedUpperPath,
			String metadataIndexId, String lastEditUser, String lastEditTime,
			String changeReason, String sharedRootId, String level);
		
	public List<ActiveHistory> getActiveHistory(List<String> sharedRootIds,String toDate,String path,boolean isFolder) ;
		
}
