package controller.impl;

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

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import service.CollaborateService;
import utils.StringUtils;
import annotation.Readonly;
import constants.HttpStatus;
import controller.CollaborateController;
import exception.MetadataException;

@RestController
public class CollaborateControllerImpl implements CollaborateController {

	@Autowired
	private CollaborateService collaborateService;

	@Override
	@RequestMapping(value = "/api/v1/collaborate/create", method = RequestMethod.POST)
	public CreateCollaborateResponse create(
			@RequestBody CreateCollaborateRequest createCollaborateRequest)
			throws MetadataException {
		if(StringUtils.isNullOrEmpty(createCollaborateRequest.getPath())){
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return collaborateService.create(createCollaborateRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/collaborate/update_member_photo", method = RequestMethod.POST)
	public void updateMemberPhoto(
			@RequestBody UpdateMemberPhotoRequest updateMemberPhotoRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateMemberPhotoRequest
				.getPhotoBlockId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		collaborateService.updateMemberPhoto(updateMemberPhotoRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/collaborate/addmember", method = RequestMethod.POST)
	public List<CollaborateMember> addMember(
			@RequestBody AddCollaborateMemberRequest addCollaborateMemberRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(addCollaborateMemberRequest.getPath())
				|| StringUtils.isNullOrEmpty(addCollaborateMemberRequest
						.getMembers())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return collaborateService.addMember(addCollaborateMemberRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/collaborate/list", method = RequestMethod.POST)
	public GetCollaborateListResponse getList(
	@RequestBody GetCollaborateRequest getCollaborateRequest)
			throws MetadataException {
		return collaborateService.getList(getCollaborateRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/collaborate/delete", method = RequestMethod.POST)
	public void delete(
			@RequestBody DeleteCollaborateRequest deleteCollaborateRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(deleteCollaborateRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		collaborateService.delete(deleteCollaborateRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/collaborate/update_member_accepted", method = RequestMethod.POST)
	public void updateMemberAccepted(
			@RequestBody UpdateMemberAcceptedRequest updateMemberAcceptedRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(updateMemberAcceptedRequest.getPath())
				|| StringUtils.isNullOrEmpty(updateMemberAcceptedRequest.getOwnerId())
				|| StringUtils.isNullOrEmpty(updateMemberAcceptedRequest.getSharedRootId())	) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		collaborateService.updateMemberAccepted(updateMemberAcceptedRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/collaborate/deletemember", method = RequestMethod.POST)
	public void deleteMember(
			@RequestBody DeleteCollaborateMemberRequest deleteCollaborateMemberRequest)
			throws MetadataException {
		collaborateService.deleteMember(deleteCollaborateMemberRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/collaborate/info", method = RequestMethod.POST)
	public GetCollaborateInfoResponse info(
			@RequestBody GetCollaborateInfoRequest getCollaborateInfoRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(getCollaborateInfoRequest.getShareRootId())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return collaborateService.info(getCollaborateInfoRequest);
	}

	@Override
	@Readonly
	@RequestMapping(value = "/api/v1/collaborate/memberinfo", method = RequestMethod.POST)
	public CollaborateMember memberInfo(
			@RequestBody GetCollaborateMemberInfoRequest getCollaborateMemberInfoRequest)
			throws MetadataException {
		return collaborateService.memberInfo(getCollaborateMemberInfoRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/collaborate/invite_member_by_mail", method = RequestMethod.POST)
	public List<InviteResponseByMail> inviteMemberByMail(
			RequestBase requestBase, @RequestBody List<InviteMemberByMailRequest> inviteMemberByMailRequest)
			throws MetadataException {
		if (inviteMemberByMailRequest.isEmpty() || inviteMemberByMailRequest.size() <= 0) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return collaborateService.inviteMemberByMail(requestBase, inviteMemberByMailRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/collaborate/invite_member_by_sms", method = RequestMethod.POST)
	public List<InviteResponseBySMS> inviteMemberBySMS(
			RequestBase requestBase, @RequestBody List<InviteMemberBySMSRequest> inviteMemberBySMSRequest)
			throws MetadataException {
		if (inviteMemberBySMSRequest.isEmpty() || inviteMemberBySMSRequest.size() <= 0) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return collaborateService.inviteMemberBySMS(requestBase, inviteMemberBySMSRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/collaborate/transform_to_shared ", method = RequestMethod.POST)
	public CreateCollaborateResponse transformToShared(
			@RequestBody CreateCollaborateRequest createCollaborateRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(createCollaborateRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return collaborateService.transformToShared(createCollaborateRequest);
	}

	@Override
	@RequestMapping(value = "/api/v1/collaborate/transform_to_normal", method = RequestMethod.POST)
	public void transformToNormal(
			@RequestBody CancelCollaborateRequest cancelCollaborateRequest)
			throws MetadataException {
		if (StringUtils.isNullOrEmpty(cancelCollaborateRequest.getPath())) {
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		collaborateService.transformToNormal(cancelCollaborateRequest);
	}

}
