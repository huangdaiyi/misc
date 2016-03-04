package controller;

import model.request.AddShareLinkRequest;
import model.request.ConfirmShareLinkRequest;
import model.request.DeleteShareLinkRequest;
import model.request.ExistsShareLinkRequest;
import model.request.GetClientVersionRequest;
import model.request.GetShareLinkRequest;
import model.request.SendAllMetadataToMQRequest;
import model.request.UpdateClientVersionRequest;
import model.request.UpdateShareLinkRequest;
import model.request.UpdateUserDiskSpaceRequest;
import model.response.AddShareLinkResponse;
import model.response.ClientVersionResponse;
import model.response.ConfirmShareLinkResponse;
import model.response.ExistsShareLinkResponse;
import model.response.GetShareLinkResponse;
import exception.MetadataException;

public interface AdminController {

	public AddShareLinkResponse addShareLink(AddShareLinkRequest request) throws MetadataException;

	public GetShareLinkResponse getShareLink(GetShareLinkRequest getShareLinkRequest) throws MetadataException;

	public ExistsShareLinkResponse existsShareLink(ExistsShareLinkRequest existsShareLinkRequest) throws MetadataException;

	public ConfirmShareLinkResponse confirmShareLink(ConfirmShareLinkRequest cnfirmShareLinkRequest) throws MetadataException;

	public void deleteShareLink(DeleteShareLinkRequest deleteShareLinkRequest) throws MetadataException;

	public void updateShareLink(UpdateShareLinkRequest updateShareLinkRequest) throws MetadataException;

	public void sendAllMetadataToMQ(SendAllMetadataToMQRequest sendAllMetadataToMQRequest) throws MetadataException;
	
	public ClientVersionResponse getClientVersion(GetClientVersionRequest getClientVersionRequest) throws MetadataException;
	
	public void updateClientVersion(UpdateClientVersionRequest updateClientVersionRequest) throws MetadataException;
	
	public void updateUserDiskSpace(UpdateUserDiskSpaceRequest updateUserDiskSpaceRequest) throws MetadataException;
}
