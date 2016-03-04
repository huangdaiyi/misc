package service;

import java.util.List;
import java.util.Map;

import model.CellPhone;
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

public interface CollaborateService {

	public CreateCollaborateResponse create(CreateCollaborateRequest createCollaborateRequest) throws MetadataException;
	
	public CreateCollaborateResponse transformToShared(CreateCollaborateRequest createCollaborateRequest) throws MetadataException;

	public Boolean updateMemberPhoto(UpdateMemberPhotoRequest updateMemberPhotoRequest) throws MetadataException;

	public List<CollaborateMember> addMember(AddCollaborateMemberRequest addCollaborateMemberRequest) throws MetadataException;

	public GetCollaborateListResponse getList(GetCollaborateRequest getCollaborateRequest) throws MetadataException;

	public Boolean delete(DeleteCollaborateRequest deleteCollaborateRequest) throws MetadataException;

	public Boolean updateMemberAccepted(UpdateMemberAcceptedRequest updateMemberAcceptedRequest) throws MetadataException;

	public Boolean deleteMember(DeleteCollaborateMemberRequest deleteCollaborateMemberRequest) throws MetadataException;

	public GetCollaborateInfoResponse info(GetCollaborateInfoRequest getCollaborateInfoRequest) throws MetadataException;

	public CollaborateMember memberInfo(GetCollaborateMemberInfoRequest getCollaborateMemberInfoRequest) throws MetadataException;

	public List<InviteResponseByMail> inviteMemberByMail(RequestBase requestBase, List<InviteMemberByMailRequest> inviteMemberByMailRequest) throws MetadataException;

	public List<InviteResponseBySMS> inviteMemberBySMS(RequestBase requestBase, List<InviteMemberBySMSRequest> inviteMemberBySMSRequest) throws MetadataException;

	public boolean canAccessCollaborate(String sharedRootId, String ownerId,String userId,String path);
	
	public void transformToNormal(CancelCollaborateRequest cancelCollaborateRequest) throws MetadataException;

	public Map<String, String> findCellphoneDisplayNameMap(List<CellPhone> cellphones);

	public void updateMemberDisplayNameFromSSO(List<CollaborateMember> members);

}
