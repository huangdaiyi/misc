package service;

import model.request.AddShareLinkRequest;
import model.request.ConfirmShareLinkRequest;
import model.request.DeleteShareLinkRequest;
import model.request.ExistsShareLinkRequest;
import model.request.GetShareLinkRequest;
import model.request.UpdateShareLinkRequest;
import model.response.AddShareLinkResponse;
import model.response.ConfirmShareLinkResponse;
import model.response.ExistsShareLinkResponse;
import model.response.GetShareLinkResponse;
import exception.MetadataException;

public interface ShareLinkService {
	public AddShareLinkResponse addShareLink(AddShareLinkRequest request) throws MetadataException;
	public GetShareLinkResponse getShareLink(GetShareLinkRequest getShareLinkRequest) throws MetadataException;
	public ExistsShareLinkResponse existsShareLink(ExistsShareLinkRequest existsShareLinkRequest) throws MetadataException;
	public ConfirmShareLinkResponse confirmShareLink(ConfirmShareLinkRequest cnfirmShareLinkRequest) throws MetadataException;
	public void deleteShareLink(DeleteShareLinkRequest deleteShareLinkRequest) throws MetadataException;
	public Boolean updateShareLink(UpdateShareLinkRequest updateShareLinkRequest) throws MetadataException;
}
