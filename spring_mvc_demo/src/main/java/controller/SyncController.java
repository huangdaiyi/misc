package controller;

import java.util.List;

import model.RequestBase;
import model.request.AddGlobalAppRequest;
import model.request.DeleteGlobalAppRequest;
import model.request.DeleteUserAppRequest;
import model.request.GetActivityHistoryRequest;
import model.request.GetUserAppRequest;
import model.request.SwitchAppSyncableRequest;
import model.request.UpdateUserAppRequest;
import model.response.GetActivityHistoryResponse;
import model.response.GetGlobalAppResponse;
import model.response.GetUserAppResponse;
import exception.MetadataException;

public interface SyncController {

	public void addGlobalApp(AddGlobalAppRequest addGlobalAppRequest) throws MetadataException;
	
	public List<GetGlobalAppResponse> getGlobalApp(RequestBase reseteGlobalIconRequest) throws MetadataException;

	public void deleteGlobalApp(DeleteGlobalAppRequest deleteGlobalAppRequest) throws MetadataException;

    public void setUserApp(UpdateUserAppRequest updateUserAppRequest) throws MetadataException;
    
    public List<GetUserAppResponse> getUserApp(GetUserAppRequest getUserAppRequest) throws MetadataException;
    
    public void deleteUserApp(DeleteUserAppRequest deleteUserAppRequest) throws MetadataException;
    
    public void switchAppSyncable(SwitchAppSyncableRequest switchAppSyncableRequest) throws MetadataException;
    
    public GetActivityHistoryResponse searchHistory( GetActivityHistoryRequest getActivityHistoryRequest) throws MetadataException;
    
    
}