package controller;

import java.util.List;

import model.CollaborateMember;
import model.RequestBase;
import model.request.AddCollaborateMemberRequest;
import model.request.CancelCollaborateRequest;
import model.request.CreateCollaborateRequest;
import model.request.DeleteCollaborateMemberRequest;
import model.request.DeleteCollaborateRequest;
import model.request.GetCollaborateInfoRequest;
import model.request.GetCollaborateMemberInfoRequest;
import model.request.GetCollaborateRequest;
import model.request.InviteMemberByMailRequest;
import model.request.InviteMemberBySMSRequest;
import model.request.UpdateMemberAcceptedRequest;
import model.request.UpdateMemberPhotoRequest;
import model.response.CreateCollaborateResponse;
import model.response.GetCollaborateInfoResponse;
import model.response.GetCollaborateListResponse;
import model.response.InviteResponseByMail;
import model.response.InviteResponseBySMS;
import exception.MetadataException;

public interface CollaborateController {

	public CreateCollaborateResponse create(CreateCollaborateRequest createCollaborateRequest) throws MetadataException;

	public GetCollaborateListResponse getList(GetCollaborateRequest getCollaborateRequest) throws MetadataException;

	public void delete(DeleteCollaborateRequest deleteCollaborateRequest) throws MetadataException;

	public GetCollaborateInfoResponse info(GetCollaborateInfoRequest getCollaborateInfoRequest) throws MetadataException;

	public CollaborateMember memberInfo(GetCollaborateMemberInfoRequest getCollaborateMemberInfoRequest) throws MetadataException;

	public List<CollaborateMember> addMember(AddCollaborateMemberRequest addCollaborateMemberRequest) throws MetadataException;

	public void updateMemberPhoto(UpdateMemberPhotoRequest updateMemberPhotoRequest) throws MetadataException;

	public void deleteMember(DeleteCollaborateMemberRequest deleteCollaborateMemberRequest) throws MetadataException;

	public void updateMemberAccepted(UpdateMemberAcceptedRequest updateMemberAcceptedRequest) throws MetadataException;

	public List<InviteResponseByMail> inviteMemberByMail(RequestBase requestBase, List<InviteMemberByMailRequest> inviteMemberByMailRequest) throws MetadataException;
	
	public List<InviteResponseBySMS> inviteMemberBySMS(RequestBase requestBase, List<InviteMemberBySMSRequest> inviteMemberBySMSRequest) throws MetadataException;
	
	public CreateCollaborateResponse transformToShared(CreateCollaborateRequest createCollaborateRequest) throws MetadataException;
	
	public void transformToNormal(CancelCollaborateRequest cancelCollaborateRequest) throws MetadataException;

}