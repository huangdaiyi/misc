package controller.impl;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
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
import model.request.UpdateMemberAcceptedRequest;
import model.request.UpdateMemberPhotoRequest;
import model.response.CreateCollaborateResponse;
import model.response.GetCollaborateInfoResponse;
import model.response.GetCollaborateListResponse;
import model.response.InviteResponseByMail;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import service.CollaborateService;
import constants.HttpStatus;
import exception.MetadataException;

public class CollaborateControllerImplTest {
	@Mock
	private CollaborateService service;
	
	@InjectMocks
	private CollaborateControllerImpl impl;
	
	@Before
	public void initMocks(){
		MockitoAnnotations.initMocks(this);
	}
	

	@Test
	public void create_BAD_REQUEST_Test(){
		HttpStatus status = HttpStatus.OK;
		CreateCollaborateRequest request = new CreateCollaborateRequest();
		
		try{
			impl.create(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
		
		status = HttpStatus.OK;
	
		request.setPath("roger");
		
		try{
			impl.create(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void createTest(){
		CreateCollaborateRequest request = new CreateCollaborateRequest();
		request.setPath("roger");
		CollaborateMember collaborateMember = new CollaborateMember();
		List<CollaborateMember> members = new ArrayList<CollaborateMember>();
		members.add(collaborateMember);
		request.setMembers(members);
		
		CreateCollaborateResponse returnResposne = Mockito.mock(CreateCollaborateResponse.class);
		
		Mockito.when(service.create(request)).thenReturn(returnResposne);
		CreateCollaborateResponse response = impl.create(request);
		assertEquals(returnResposne,response);
	}
	@Test
	public void transformTest(){
		CreateCollaborateRequest request = new CreateCollaborateRequest();
		request.setPath("roger");
		CollaborateMember collaborateMember = new CollaborateMember();
		List<CollaborateMember> members = new ArrayList<CollaborateMember>();
		members.add(collaborateMember);
		request.setMembers(members);
		
		CreateCollaborateResponse returnResposne = Mockito.mock(CreateCollaborateResponse.class);
		
		Mockito.when(service.transformToShared(request)).thenReturn(returnResposne);
		CreateCollaborateResponse response = impl.transformToShared(request);
		assertEquals(returnResposne,response);
	}
	
	@Test
	public void transformToNormal_BAD_REQUEST_Test(){
		HttpStatus status = HttpStatus.OK;
		CancelCollaborateRequest request = Mockito.mock(CancelCollaborateRequest.class);
		try{
			impl.transformToNormal(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
		
	
	@Test
	public void transformToNormal_Test(){
		CancelCollaborateRequest request = Mockito.mock(CancelCollaborateRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		Mockito.when(request.getOwnerId()).thenReturn("roger");	
		impl.transformToNormal(request);
	}
	
	@Test
	public void updateMemberPhotoRequest_BAD_REQUEST_Test(){
		HttpStatus status = HttpStatus.OK;
		UpdateMemberPhotoRequest request = Mockito.mock(UpdateMemberPhotoRequest.class);
		
		try{
			impl.updateMemberPhoto(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void updateMemberPhotoRequest_Test(){
		UpdateMemberPhotoRequest request = Mockito.mock(UpdateMemberPhotoRequest.class);
		Mockito.when(request.getPhotoBlockId()).thenReturn("roger");
		
		
		Mockito.when(service.updateMemberPhoto(request)).thenReturn(true);
		impl.updateMemberPhoto(request);
	}
	
	@Test
	public void addMember_BAD_REQUEST_Test(){
		HttpStatus status = HttpStatus.OK;
		AddCollaborateMemberRequest request = Mockito.mock(AddCollaborateMemberRequest.class);
		
		try{
			impl.addMember(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void addMember_Test(){
		AddCollaborateMemberRequest request = new AddCollaborateMemberRequest();
		request.setPath("roger");
		CollaborateMember collaborateMember = new CollaborateMember();
		List<CollaborateMember> members = new ArrayList<CollaborateMember>();
		members.add(collaborateMember);
		request.setMembers(members);
		
		List<CollaborateMember> returnResposne = new ArrayList<CollaborateMember>();
		
		Mockito.when(service.addMember(request)).thenReturn(returnResposne);
		List<CollaborateMember> response = impl.addMember(request);
		assertEquals(returnResposne,response);
	}
	
	@Test
	public void getCollaborate_Test(){

		GetCollaborateRequest request = new GetCollaborateRequest();
		
		GetCollaborateListResponse returnResposne = new GetCollaborateListResponse();
		
		Mockito.when(service.getList(request)).thenReturn(returnResposne);
		GetCollaborateListResponse response = impl.getList(request);
		assertEquals(returnResposne,response);
	}
	
	@Test
	public void delete_BAD_REQUEST_Test(){
		HttpStatus status = HttpStatus.OK;
		DeleteCollaborateRequest request = Mockito.mock(DeleteCollaborateRequest.class);
		
		try{
			impl.delete(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void delete_Test(){
		DeleteCollaborateRequest request = Mockito.mock(DeleteCollaborateRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		Mockito.when(service.delete(request)).thenReturn(true);
		impl.delete(request);
	}
	
	@Test
	public void updateMemberAccepted_BAD_REQUEST_Test(){
		HttpStatus status = HttpStatus.OK;
		UpdateMemberAcceptedRequest request = Mockito.mock(UpdateMemberAcceptedRequest.class);
		
		try{
			impl.updateMemberAccepted(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void updateMemberAccepted_Test(){
		UpdateMemberAcceptedRequest request = Mockito.mock(UpdateMemberAcceptedRequest.class);
		Mockito.when(request.getPath()).thenReturn("roger");
		
		
		Mockito.when(service.updateMemberAccepted(request)).thenReturn(true);
		impl.updateMemberAccepted(request);
	}
	
	@Test
	public void deleteMember_Test(){
		DeleteCollaborateMemberRequest request = Mockito.mock(DeleteCollaborateMemberRequest.class);	
		
		Mockito.when(service.deleteMember(request)).thenReturn(true);
		impl.deleteMember(request);
	}
	
	@Test
	public void info_BAD_REQUEST_Test(){
		HttpStatus status = HttpStatus.OK;
		GetCollaborateInfoRequest request = Mockito.mock(GetCollaborateInfoRequest.class);
		Mockito.when(request.getShareRootId()).thenReturn("roger");
		try{
			impl.info(request);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	@Test
	public void info_Test(){
		GetCollaborateInfoRequest request = new GetCollaborateInfoRequest();
		request.setShareRootId("roger");
			
		GetCollaborateInfoResponse returnResposne = new GetCollaborateInfoResponse();
		
		Mockito.when(service.info(request)).thenReturn(returnResposne);
		GetCollaborateInfoResponse response = impl.info(request);
		assertEquals(returnResposne,response);
	}
	
	@Test
	public void memberInfo_Test(){
		GetCollaborateMemberInfoRequest request = new GetCollaborateMemberInfoRequest();
		
		CollaborateMember returnResposne = new CollaborateMember();
		
		Mockito.when(service.memberInfo(request)).thenReturn(returnResposne);
		CollaborateMember response = impl.memberInfo(request);
		assertEquals(returnResposne,response);
	}
	
	@Test
	public void inviteMemberByMail_Test(){
		List<InviteMemberByMailRequest> request = new ArrayList<InviteMemberByMailRequest>();
		List<InviteResponseByMail> returnResponse = new ArrayList<InviteResponseByMail>();
		RequestBase requestBase = new RequestBase();
		
		Mockito.when(service.inviteMemberByMail(requestBase, request)).thenReturn(returnResponse);
		List<InviteResponseByMail> response = impl.inviteMemberByMail(requestBase, request);
		assertEquals(returnResponse,response);
	}
	
//	@Test
//	public void inviteMemberBySMS_Test(){
//		InviteMemberBySMSRequest request = new InviteMemberBySMSRequest();
//		
//		Mockito.when(service.inviteMemberBySMS(request)).thenReturn(true);
//		impl.inviteMemberBySMS(request);
//	}
	
}
