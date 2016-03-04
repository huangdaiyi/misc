package service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import model.CellPhone;
import model.CollaborateMap;
import model.CollaborateMember;
import model.CountManageModel;
import model.Mail;
import model.Metadata;
import model.PathInfo;
import model.PathRequestBase;
import model.RequestBase;
import model.request.AddCollaborateMemberRequest;
import model.request.CancelCollaborateRequest;
import model.request.CreateCollaborateRequest;
import model.request.DeleteCollaborateMemberRequest;
import model.request.DeleteCollaborateRequest;
import model.request.DeleteFolderRequest;
import model.request.GetCollaborateInfoRequest;
import model.request.GetCollaborateMemberInfoRequest;
import model.request.GetCollaborateRequest;
import model.request.InviteMemberByMailRequest;
import model.request.InviteMemberBySMSRequest;
import model.request.SSOFindUserIdByCellphoneReq;
import model.request.SendSMSRequest;
import model.request.UpdateMemberAcceptedRequest;
import model.request.UpdateMemberPhotoRequest;
import model.response.CreateCollaborateResponse;
import model.response.GetCollaborateInfoResponse;
import model.response.GetCollaborateListResponse;
import model.response.InviteResponseByMail;
import model.response.InviteResponseBySMS;
import model.response.SSOFindUserIdByCellphone;
import model.response.SSOUserInfoResponse;
import model.response.SendSMSResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import service.CollaborateService;
import service.CountService;
import service.DynamoDBService;
import service.EmailService;
import service.FileService;
import service.FolderService;
import service.SSOService;
import service.XmppService;
import utils.CellphonesUtils;
import utils.DateUtils;
import utils.MathUtils;
import utils.StringUtils;
import auth.CollaborateAuth;

import constants.ActivityHistoryLevel;
import constants.ChangeReason;
import constants.HttpStatus;
import constants.InviteType;
import constants.MetadataType;
import constants.SortType;
import constants.SystemEvent;
import dao.CollaborateDao;
import dao.MetadataDao;
import exception.MetadataException;
import factory.MetadataFactory;
import factory.PathFactory;

@Service
public class CollaborateServiceImpl implements CollaborateService {

	private static final Logger logger = LogManager
			.getLogger(CollaborateServiceImpl.class);
	
	@Autowired
	private MetadataDao metadataDao;
	@Autowired
	private CollaborateDao collaborateDao;
	@Autowired
	private FileService fileService;
	@Autowired
	private FolderService folderService;
	@Autowired
	private PathFactory pathFactory;
	@Autowired
	private MetadataFactory metadataFactory;
	@Autowired
	private EmailService emailService;
	@Autowired
	private SSOService ssoService;
	@Autowired
	private XmppService xmppService;
	@Autowired
	private DynamoDBService dynamoDBService;
	@Autowired
	private CountService countService;

	@Value("${neweggbox.portal}")
	private String webPortal;
	@Value("${gateway.external.url}")
	private String gatewayExternalUrl;
	@Value("${collaborate.inviteMailTemplate}")
	private String collaborateInviteMailTemplate;
	@Value("${collaborate.subject}")
	private String collaborateInviteMailSubjectTemplate;
	@Value("${collaborate.inviteSMSTemplate}")
	private String collaborateInviteSMSTemplate;
	@Value("${box.installer.download.url}")
	private String installDownloadUrl;

	@Override
	@Transactional
	public CreateCollaborateResponse create(CreateCollaborateRequest createCollaborateRequest) throws MetadataException {
		pathFactory.checkCanCreateCollaborate(createCollaborateRequest.getPath());
		PathInfo pathInfo = pathFactory.parsePathInfo(createCollaborateRequest, false, false);

		String parentPath = FilenameUtils.getFullPathNoEndSeparator(pathInfo.getFullOriginPath());
		Metadata parentFolder = metadataDao.getMetadataByPath(parentPath, "", "");
		if (parentFolder == null) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}
		// check upper level
		checkUpperType(parentFolder, pathInfo.getUserId());

		String ownerId = createCollaborateRequest.getUserId();

		createCollaborateRequest.setOwnerId(ownerId);
		folderService.createFolder(createCollaborateRequest);
		Metadata collaborateFolder = metadataDao.getMetadataByPath(pathInfo.getFullOriginPath(), "", "");
		metadataDao.updateSharedRootId(collaborateFolder.getId());
		metadataDao.updateMetadataType(collaborateFolder.getId(), MetadataType.SHARE);
		collaborateDao.createCollaborate(collaborateFolder.getId());
		List<CollaborateMember> withMembers = createCollaborateRequest.getMembers();

		pathInfo.setSharedRootId(collaborateFolder.getId());
		pathInfo.setUnderSharedFolders(true);
		
		CreateCollaborateResponse collaborateResponse = new CreateCollaborateResponse();
		collaborateResponse.setOwnerId(createCollaborateRequest.getUserId());
		collaborateResponse.setName(collaborateFolder.getOriginName());
		collaborateResponse.setPath(createCollaborateRequest.getPath());
		collaborateResponse.setSharedRootId(collaborateFolder.getId());
		String displayName = ssoService.findDisplayNameByUserId(createCollaborateRequest.getUserId());
		collaborateResponse.setOwner(displayName);
		
		if (!StringUtils.isNullOrEmpty(withMembers)) {
			List<CollaborateMember> createdMembers = new ArrayList<CollaborateMember>();
			for (CollaborateMember member : withMembers) {
				createdMembers.add(createCollaborateMember(createCollaborateRequest.getUserId(),pathInfo.getFullOwnerPath(), collaborateFolder, member));
			}
			// invite unregistered member
		    updateMemberDisplayNameFromSSO(createdMembers);
		    inviteUnregisteredMembers(createCollaborateRequest.getToken(), createCollaborateRequest.getUserId(),createdMembers);
			collaborateResponse.setMembers(createdMembers);
		}
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.addFolder(collaborateFolder.getId(), 0, 0, 0, 1, 0,0);
		countService.updateAllParentCount(countManageModel);
		// activity history
		dynamoDBService.createActivityHistory(pathInfo.getFullOwnerPath(), "",
				pathInfo.getFullOwnerPath(), "", collaborateFolder.getId(),
				ownerId, DateUtils.nowUTCDateTime(),
				ChangeReason.CREATE_COLLABORATE.toString(),
				collaborateFolder.getId(),
				ActivityHistoryLevel.NORMAL.toString());
		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.UPDATE_COLLABORATE, pathInfo, null, true, false);
		return collaborateResponse;

	}

	@Override
	public Boolean updateMemberPhoto(UpdateMemberPhotoRequest updateMemberPhotoRequest) throws MetadataException {
		String photoBlockId = updateMemberPhotoRequest.getPhotoBlockId();
		int photoSize = updateMemberPhotoRequest.getPhotoSize();
		long memberId = updateMemberPhotoRequest.getMemberId();
		String userId = updateMemberPhotoRequest.getUserId();
		CollaborateMap collaborateMap = getCollaborateMapByMemberId(memberId);
		Metadata collaborateFolder = collaborateMap.getCollaborateFolder();
		CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, userId);
		if (collaborateAuth.isOwner()) {
			if (collaborateDao.updateCollaborateMemberPhoto(memberId, photoBlockId, photoSize)) {
				String fullPath = metadataDao.getFullPath(collaborateFolder.getId());
				// create activity
				sendActivityHistory(collaborateFolder,fullPath, userId, ChangeReason.UPDATE_MEMBER_PHOTO);
				// send xmpp
				PathInfo pathInfo = new PathInfo(userId, collaborateFolder.getOwnerId(), fullPath, collaborateFolder.getSharedRootId(), true);
				pathInfo.setFullOriginPath(fullPath);
				xmppService.sendToAllDevices(SystemEvent.UPDATE_MEMBER_PHOTO, pathInfo);
				return true;
			}
		}
		return false;
	}

	@Override
	public List<CollaborateMember> addMember(AddCollaborateMemberRequest addCollaborateMemberRequest) throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(addCollaborateMemberRequest, false, true);
		Metadata collaborateFolder = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), "", "");
		CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, pathInfo.getUserId(), pathInfo.getOwnerId());
		List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(collaborateFolder.getSharedRootId());
		List<CellPhone> userCellPhones = dynamoDBService.findCellphonesByUserId(pathInfo.getUserId());
		if (!collaborateAuth.isMember(members, userCellPhones) && !collaborateAuth.isOwner()) {
			throw new MetadataException(HttpStatus.CANT_ACCESS_COLLABORATE);
		}
		members = new ArrayList<CollaborateMember>();
		for (CollaborateMember member : addCollaborateMemberRequest.getMembers()) {
			members.add(createCollaborateMember(addCollaborateMemberRequest.getUserId(), pathInfo.getFullOwnerPath(), collaborateFolder, member));
		}
		// invite unregistered member
		updateMemberDisplayNameFromSSO(members);
		inviteUnregisteredMembers(addCollaborateMemberRequest.getToken(), addCollaborateMemberRequest.getUserId(),members);

		// send XMPP
		xmppService.sendToAllDevices(SystemEvent.UPDATE_COLLABORATE_MEMBER, pathInfo);
		for (CollaborateMember member : members) {
			if (member.getAccepted() != null || member.isExists() == false) {
				continue;
			}
			Collection<String> userIds = ssoService.findCellphoneUserIdMap(member.getCellphones()).values();
			for (String memberUserId : userIds) {
				PathInfo childPathInfo = pathInfo.clone();
				childPathInfo.setUserId(memberUserId);
				// invite member message for this time
				xmppService.sendToAllDevices(SystemEvent.INVITE_COLLABORATE_MEMBER, childPathInfo, null, false, false);
			}
		}
		return members;
	}

	@Override
	public GetCollaborateListResponse getList(GetCollaborateRequest getCollaborateRequest) throws MetadataException {
		String userId = getCollaborateRequest.getUserId();
		List<GetCollaborateInfoResponse> folders = new ArrayList<GetCollaborateInfoResponse>();
		// user = owner
		List<Metadata> selfShared = metadataDao.getMetadatasByType(userId, MetadataType.SHARE);
		if (getCollaborateRequest.isIncludeOwn()) {
			for (Metadata metadata : selfShared) {
				if (getCollaborateRequest.isIncludeInvisibleFolder() == false && metadata.isVisible() == false) {
					continue;
				}
				List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(metadata.getId());
				GetCollaborateInfoResponse getCollaborateInfoResponse = metadataFactory.toGetCollaborateInfoResponse(metadata, userId);
				getCollaborateInfoResponse.setupMembers(members, null, getCollaborateRequest.isDetail());
				folders.add(getCollaborateInfoResponse);
			}
		}

		// user = member
		List<CellPhone> inquirerCellphones = dynamoDBService.findCellphonesByUserId(getCollaborateRequest.getUserId());
		String cellphones = CellphonesUtils.composeCellphones(inquirerCellphones);
		List<CollaborateMember> allMyCollaborateMembers = collaborateDao.getCollaborateMembersByCellphones(cellphones, false, false, false);
		for (CollaborateMember myCollaborateMember : allMyCollaborateMembers) {
			Metadata sharedWithUser = metadataDao.getMetadata(myCollaborateMember.getMetadataIndexId());
			if (sharedWithUser == null) {
				continue;
			}
			List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(sharedWithUser.getId());
			GetCollaborateInfoResponse getCollaborateInfoResponse = metadataFactory.toGetCollaborateInfoResponse(sharedWithUser, userId);
			getCollaborateInfoResponse.setupMembers(members, myCollaborateMember, getCollaborateRequest.isDetail());
			folders.add(getCollaborateInfoResponse);
		}

		metadataFactory.sortCollaborateInfoResponses(folders, SortType.NAME);
		folders = metadataFactory.pagingFolderResponses(folders, getCollaborateRequest.getPageNumber(), getCollaborateRequest.getItemsPerPage(), 0);
		metadataFactory.updateGetCollaborateInfoResponseAfterPaging(folders, userId);
		
		GetCollaborateListResponse getCollaborateListResponse = new GetCollaborateListResponse();
		getCollaborateListResponse.setFolders(folders);
		getCollaborateListResponse.setSortByType(SortType.NAME);
		getCollaborateListResponse.setTotalPages(MathUtils.ceil(folders.size(), getCollaborateRequest.getItemsPerPage(), 1));
		return getCollaborateListResponse;
	}

	@Override
	@Transactional
	public Boolean delete(DeleteCollaborateRequest deleteCollaborateRequest) throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(deleteCollaborateRequest, false, true);
		String ownerId = StringUtils.isNullOrEmpty(deleteCollaborateRequest.getOwnerId()) ? deleteCollaborateRequest.getUserId() : deleteCollaborateRequest.getOwnerId();
		String userId = deleteCollaborateRequest.getUserId();
		Metadata collaborateFolder = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), "", "");
		CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, userId, ownerId);
		List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(collaborateFolder.getId());
		List<CellPhone> userCellPhones = dynamoDBService.findCellphonesByUserId(userId);
		if (collaborateAuth.isOwner() == false && collaborateAuth.isMember(members, userCellPhones) == false) {
			return false;
		}
		// owner
		// update collaborate is_delete = 1
		collaborateDao.deleteCollaborate(collaborateFolder.getId());
		// delete folder
		DeleteFolderRequest deleteFolderRequest = new DeleteFolderRequest();
		deleteFolderRequest.setUserId(userId);
		deleteFolderRequest.setOwnerId(ownerId);
		deleteFolderRequest.setPath(pathInfo.getFullOwnerPath());
		deleteFolderRequest.setExtract(false);
		deleteFolderRequest.setToken(deleteCollaborateRequest.getToken());
		folderService.deleteFolderForSuccessValidate(deleteFolderRequest, pathInfo);
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.addFolder(collaborateFolder.getId(), 0, 0, 0, -1, 0,0);
		countService.updateAllParentCount(countManageModel);
		//activity history
		sendActivityHistory(collaborateFolder, pathInfo.getFullOwnerPath(), pathInfo.getOwnerId(), ChangeReason.DELETE_COLLABORATE);
		// send xmpp
		xmppService.sendToAllDevices(SystemEvent.DELETE_COLLABORATE, pathInfo);
		return true;
	}

	@Override
	@Transactional
	public void transformToNormal(CancelCollaborateRequest cancelCollaborateRequest) throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(cancelCollaborateRequest, false, true);
		Metadata collaborateFolderMetadata = metadataDao.getMetadataByPath(pathInfo.getFullOwnerPath(), pathInfo.getDeviceUid(), pathInfo.getFullSourcePath());
		if (collaborateFolderMetadata == null) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}
		
		// return file to file owner(member) if fileReturn = true
		if(cancelCollaborateRequest.isReturnToCreator()){
			returnFileOrFolderToMember(collaborateFolderMetadata);
		}else{
			// find all sub files and folders in shared folder
			List<Metadata> allSubFoldersAndFilesMetadatas = metadataFactory.getAllSubFoldersAndFiles(collaborateFolderMetadata);
			for(Metadata metadata : allSubFoldersAndFilesMetadatas){
				// update shared root id as ""
				collaborateDao.cleanSharedRootId(metadata.getSharedRootId());
			}
		}
		// update shared folder's metadata
		collaborateFolderMetadata.setSharedRootId("");
		collaborateFolderMetadata.setType(MetadataType.NORMAL.toString());
		metadataDao.updateFileMetadata(collaborateFolderMetadata);
		// update collaborate is_delete = 1
		collaborateDao.deleteCollaborate(collaborateFolderMetadata.getId());

		CountManageModel countManageModel = new CountManageModel();
		countManageModel.addFolder(collaborateFolderMetadata.getId(), 0, 0, 0, -1, 0,0);
		countService.updateAllParentCount(countManageModel);
		
		// send xmpp to owner
		xmppService.sendToAllDevices(SystemEvent.CANCEL_COLLABORATE, pathInfo);

	}
	
	private void returnFileOrFolderToMember(Metadata collaborateFolderMetadata){
		// find all sub files and folders in shared folder
		List<Metadata> allSubFoldersAndFilesMetadatas = metadataFactory.getAllSubFoldersAndFiles(collaborateFolderMetadata);
		// return file/folder which is created by member
		HashMap<String, String> mapMemberId = new HashMap<String, String>();
		HashMap<String, String> refMapId = new HashMap<String, String>();
		// find type = reflink under this shared folder
		List<String> sharedIds = new ArrayList<String>();
		sharedIds.add(collaborateFolderMetadata.getSharedRootId());
		List<Metadata> reflinkMetadata = metadataDao.getRefMetadataBySharedId(sharedIds);
		if (reflinkMetadata != null){
			for(Metadata item : reflinkMetadata){
				String newSharedFolderPath = metadataFactory.getMetadataOriginalPath(item); // exclude ownerId
																					 // from destination folder to shared folder
																					 // ex: aaa/sharedFolder
				String newParentPath = newSharedFolderPath.substring(0, newSharedFolderPath.lastIndexOf("/"));
				refMapId.put(item.getOwnerId(), newParentPath);
				metadataDao.deleteMetadata(item.getId());

			}
		}
		
		for (Metadata metadata : allSubFoldersAndFilesMetadatas) {
			// update shared root id as ""
			collaborateDao.cleanSharedRootId(metadata.getSharedRootId());
			// return file/folder to member
			if (!metadata.getCreatedBy().equals(metadata.getOwnerId())) {
				String nodePath = metadataFactory.getMetadataOriginalPath(metadata);// exclude
																					// ownerId

				// create folder under root for each member at first time
				if (!mapMemberId.keySet().contains(metadata.getCreatedBy())){
					// check if there exists the same name folder
					String folderFullPath = refMapId.size()==0 ? new StringBuilder().append(metadata.getCreatedBy())
							.append("/").append(nodePath.substring(0, nodePath.indexOf("/"))).toString() : 
								new StringBuilder().append(metadata.getCreatedBy())
								.append("/").append(refMapId.get(metadata.getCreatedBy())).append("/").append(nodePath.substring(0, nodePath.indexOf("/"))).toString();
					Metadata folder = metadataDao.getMetadataByPath(folderFullPath, "", ""); 
					String folderPath = folder == null ? nodePath.substring(0, nodePath.indexOf("/")) : metadataFactory.generateNameForMoveCopy(folder, true, false);
					folderFullPath = folder == null ? new StringBuilder().append(metadata.getCreatedBy()).append("/").append(nodePath.substring(0, nodePath.indexOf("/"))).toString() : new StringBuilder().append(metadata.getCreatedBy()).append("/")
							.append(metadataFactory.generateNameForMoveCopy(folder, true, false)).toString();
					if (refMapId.size()>0){
						folderPath = new StringBuilder().append(refMapId.get(metadata.getCreatedBy())).append("/").append(folderPath).toString();
						folderFullPath = new StringBuilder().append(metadata.getCreatedBy()).append("/").append(folderPath).toString();
						refMapId.remove(metadata.getCreatedBy());
					}
					
					PathRequestBase request = new PathRequestBase();
					request.setUserId(metadata.getCreatedBy());
					request.setPath(folderPath);
					request.setSourcePath("");
					folderService.createFolder(request);
					Metadata newFolder = metadataDao.getMetadataByPath(folderFullPath, "", "");
					// update note and icon
					metadataDao.updateNote(newFolder.getId(), metadata.getNote(), DateUtils.nowUTCTimestamp(), metadata.getCreatedBy());
					metadataDao.updateMetadataIcon(newFolder.getId(), metadata.getIconBlockId(), metadata.getIconText(), metadata.getIconTextColor(), metadata.getIconTextStyle());
					// update hashMap
					mapMemberId.put(metadata.getCreatedBy(), folderPath);
				}			
				nodePath = mapMemberId.get(metadata.getCreatedBy()) + nodePath.substring(nodePath.indexOf("/"));
				String memberFullPath = new StringBuilder().append(metadata.getCreatedBy()).append("/").append(nodePath).toString();
				if (!metadata.isFolder()) {
					nodePath = nodePath.substring(0, nodePath.lastIndexOf("/"));
					memberFullPath = memberFullPath.substring(0, memberFullPath.lastIndexOf("/"));
				}
				metadataFactory.recursiveCopyAccordingToPath(metadata, metadata.getCreatedBy(), nodePath);
				if (!metadata.isFolder()) {
					Metadata parentFolder = metadataDao.getMetadataByPath(memberFullPath, "", "");
					fileService.copySingleFile(metadata, null, parentFolder, metadata.getCreatedBy(), metadata.getCreatedBy(), false, false);
				}
			}
		}
	}

	@Override
	@Transactional
	public Boolean updateMemberAccepted(UpdateMemberAcceptedRequest updateMemberAcceptedRequest) throws MetadataException {
		String ownerId = updateMemberAcceptedRequest.getOwnerId();
		String userId = updateMemberAcceptedRequest.getUserId();
		Metadata collaborateFolder = metadataDao.getMetadata(updateMemberAcceptedRequest.getSharedRootId());
		CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, userId, ownerId);
		collaborateAuth.setAccept(false);
		if (collaborateAuth.isOwner()) {
			// member only
			throw new MetadataException(HttpStatus.CANT_ACCESS_COLLABORATE);
		}
		List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(collaborateFolder.getId());
		List<CellPhone> userCellPhones = dynamoDBService.findCellphonesByUserId(userId);
		if (collaborateAuth.isMember(members, userCellPhones)) {
			for (CellPhone cellPhone : userCellPhones) {
				collaborateDao.updateMemberAccepted(updateMemberAcceptedRequest.isAccepted(), collaborateFolder.getId(), cellPhone.toString());
			}
			String fullPath = metadataDao.getFullPath(collaborateFolder.getId());
			ChangeReason reason = null;
			if (updateMemberAcceptedRequest.isAccepted() == null) {
				reason = ChangeReason.WAIT_FOR_INVITATION;
			}else	if (updateMemberAcceptedRequest.isAccepted()) {
				reason = ChangeReason.ACCEPT_THE_INVITATION;
			}else {
				reason = ChangeReason.REJECT_THE_INVITATION;
			}
			// create activity
			sendActivityHistory(collaborateFolder,fullPath, userId, reason);
			// send xmpp
			PathInfo pathInfo = new PathInfo(userId, collaborateFolder.getOwnerId(), fullPath, collaborateFolder.getId(), true);
			String fullOriginPath = updateMemberAcceptedRequest.getUserId()+"/"+updateMemberAcceptedRequest.getPath();
			pathInfo.setFullOriginPath(fullOriginPath);
			pathInfo.setUnderSharedFolders(true);
			xmppService.sendToAllDevices(SystemEvent.UPDATE_COLLABORATE_MEMBER, pathInfo);
		} else {
			throw new MetadataException(HttpStatus.CANT_ACCESS_COLLABORATE);
		}

		return true;
	}

	@Override
	public Boolean deleteMember(DeleteCollaborateMemberRequest deleteCollaborateMemberRequest) throws MetadataException {
		long memberId = deleteCollaborateMemberRequest.getMemberId();
		String userId = deleteCollaborateMemberRequest.getUserId();
		CollaborateMap collaborateMap = getCollaborateMapByMemberId(memberId);
		Metadata collaborateFolder = collaborateMap.getCollaborateFolder();
		List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(collaborateFolder.getId());
		List<CellPhone> userCellPhones = dynamoDBService.findCellphonesByUserId(userId);
		CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, userId, collaborateFolder.getOwnerId());
		if (collaborateAuth.isOwner() || collaborateAuth.isMember(members, userCellPhones)) {
			collaborateDao.deleteCollaborateMember(memberId);
			String fullPath = metadataDao.getFullPath(collaborateFolder.getId());
			// activity history
			sendActivityHistory(collaborateFolder, fullPath,userId,  ChangeReason.DELETE_COLLABORATE_MEMBER);
			// send xmpp
			PathInfo pathInfo = new PathInfo(userId, collaborateFolder.getOwnerId(), fullPath, collaborateFolder.getId(), true);
			xmppService.sendToAllDevices(SystemEvent.DELETE_COLLABORATE_MEMBER, pathInfo, null, true, false);
			return true;
		}
		return false;
	}

	@Override
	public GetCollaborateInfoResponse info(GetCollaborateInfoRequest getCollaborateInfoRequest) throws MetadataException {
		
		Metadata collaborateFolder = metadataDao.getMetadata(getCollaborateInfoRequest.getShareRootId());
		if (collaborateFolder == null) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}
		String userId = getCollaborateInfoRequest.getUserId();
		List<CollaborateMember> collaborateMembers = collaborateDao.getCollaborateMembersByMetadataIndexId(collaborateFolder.getId());
		List<CellPhone> userCellPhones = dynamoDBService.findCellphonesByUserId(userId);
		CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, userId, collaborateFolder.getOwnerId());
		collaborateAuth.setAccept(null);
		if (!collaborateAuth.isOwner() && !collaborateAuth.isMember(collaborateMembers, userCellPhones)) {
			throw new MetadataException(HttpStatus.CANT_ACCESS_COLLABORATE);
		}

		List<CellPhone> requesterCellphones = dynamoDBService.findCellphonesByUserId(userId);
		String cellphones = CellphonesUtils.composeCellphones(requesterCellphones);
		CollaborateMember requesterCollaborateInfo = collaborateDao.getCollaborateMemberByFragmentCellphone(collaborateFolder.getId(), cellphones);
		GetCollaborateInfoResponse getCollaborateInfoResponse = metadataFactory.toGetCollaborateInfoResponse(collaborateFolder, userId);
		getCollaborateInfoResponse.setupMembers(collaborateMembers, requesterCollaborateInfo, getCollaborateInfoRequest.isDetail());
		metadataFactory.updateGetCollaborateInfoResponseAfterPaging(Arrays.asList(getCollaborateInfoResponse), userId);
		return getCollaborateInfoResponse;
	}

	@Override
	public CollaborateMember memberInfo(GetCollaborateMemberInfoRequest getCollaborateMemberInfoRequest) throws MetadataException {
		long memberId = getCollaborateMemberInfoRequest.getMemberId();
		String userId = getCollaborateMemberInfoRequest.getUserId();
		CollaborateMap collaborateMap = getCollaborateMapByMemberId(memberId);
		CollaborateMember member = collaborateMap.getMember();
		Metadata collaborateFolder = collaborateMap.getCollaborateFolder();
		CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, userId);
		if (collaborateAuth.isOwner()) {
			if (!getCollaborateMemberInfoRequest.getDetail()) {
				// if detail = false,not display
				member.setPhotoBlockId(null);
				member.setPhotoSize(null);
			}
			// not display metadata index id
			member.setMetadataIndexId(null);
		}
		
		updateMemberDisplayNameFromSSO(Arrays.asList(member));
		
		return member;
	}

	@Override
	public List<InviteResponseByMail> inviteMemberByMail(RequestBase requestBase, List<InviteMemberByMailRequest> inviteMemberByMailRequest) throws MetadataException {
		List<InviteResponseByMail> response = new ArrayList<InviteResponseByMail>();
		for (InviteMemberByMailRequest item : inviteMemberByMailRequest) {
			try {
				InviteResponseByMail result = new InviteResponseByMail();
				result.setMemberId(item.getMemberId());
				response.add(result);

				if (item.getMemberId() <= 0) {
					continue;
				}
				CollaborateMap collaborateMap = getCollaborateMapByMemberId(item.getMemberId());
				final CollaborateMember member = collaborateMap.getMember();
				if (item.getForceMails() != null && item.getForceMails().isEmpty() == false) {
					result.setMails(item.getForceMails());
					for (Mail mail : result.getMails()) {
						mail.setMailStatusCode(HttpStatus.INVITE_STATUS_NOT_FOUND.getCode());
						for (Mail memberMail : member.getMails()) {
							if (memberMail.isMailEquals(mail)) {
								mail.setMailStatusCode(null);
								break;
							}
						}
					}
				} else {
					result.setMails(member.getMails());
				}

				// member auth
				String userId = requestBase.getUserId();
				Metadata collaborateFolder = collaborateMap.getCollaborateFolder();
				List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(collaborateFolder.getId());
				List<CellPhone> userCellPhones = dynamoDBService.findCellphonesByUserId(userId);
				CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, userId, collaborateFolder.getOwnerId());
				if (!collaborateAuth.isOwner() && !collaborateAuth.isMember(members, userCellPhones)) {
					result.setMailStatusCodeForEachMails(HttpStatus.CANT_ACCESS_COLLABORATE.getCode());
					continue;
				}

				// is registered
				SSOFindUserIdByCellphoneReq ssoFindUserIdByCellphoneReq = new SSOFindUserIdByCellphoneReq(member.getCellphones());
				SSOFindUserIdByCellphone userIds = ssoService.findUserIdByCellophones(ssoFindUserIdByCellphoneReq);
				if (userIds.getUsers().isEmpty() == false) {
					result.setMailStatusCodeForEachMails(HttpStatus.COLLABORATE_MEMBER_REGISTERED.getCode());
					continue;
				}

				// send
				SSOUserInfoResponse ownerUserInfo = ssoService.getUserInfoByToken(requestBase);
				String displayInviteCellphones = CellphonesUtils.toDisplayCellphone(member.getCellphones());
				String body = String.format(collaborateInviteMailTemplate, gatewayExternalUrl, ownerUserInfo.getName(), displayInviteCellphones, webPortal);
				String subject = String.format(collaborateInviteMailSubjectTemplate, ownerUserInfo.getName());

				for (Mail mail : result.getMails()) {
					if (mail.getMailStatusCode() != null) {
						continue;
					} else if (StringUtils.isNullOrEmpty(mail.getMail())) {
						mail.setMailStatusCode(HttpStatus.ERROR_IN_PARAMETERS.getCode());
						continue;
					}
					if (emailService.sendEmail(mail.getMail(), subject, body)) {
						mail.setMailStatusCode(HttpStatus.OK.getCode());
					} else {
						mail.setMailStatusCode(HttpStatus.INTERNAL_SERVER_ERROR.getCode());
					}
				}
				collaborateDao.updateCollaborateMemberInvitedByMail(item.getMemberId());
			} catch (Exception e) {
				logger.error("inviteMemberByMail error", e);
			}
		}
		return response;
	}

	@Override
	public List<InviteResponseBySMS> inviteMemberBySMS(RequestBase requestBase, List<InviteMemberBySMSRequest> inviteMemberBySMSRequest) throws MetadataException {
		List<InviteResponseBySMS> response = new ArrayList<InviteResponseBySMS>();
		for (InviteMemberBySMSRequest item : inviteMemberBySMSRequest) {
			try {
				InviteResponseBySMS result = new InviteResponseBySMS();
				result.setMemberId(item.getMemberId());
				response.add(result);

				if (item.getMemberId() <= 0) {
					continue;
				}
				CollaborateMap collaborateMap = getCollaborateMapByMemberId(item.getMemberId());
				final CollaborateMember member = collaborateMap.getMember();
				if (item.getForceCellphones() != null && item.getForceCellphones().isEmpty() == false) {
					result.setCellphones(item.getForceCellphones());
					for (CellPhone cellphone : result.getCellphones()) {
						cellphone.setSmsStatusCode(HttpStatus.INVITE_STATUS_NOT_FOUND.getCode());
						for (CellPhone memberCellphone : member.getCellphones()) {
							if (memberCellphone.isCellphoneEquals(cellphone)) {
								cellphone.setSmsStatusCode(null);
								break;
							}
						}
					}
				} else {
					result.setCellphones(member.getCellphones());
				}

				// member auth
				String userId = requestBase.getUserId();
				Metadata collaborateFolder = collaborateMap.getCollaborateFolder();
				List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(collaborateFolder.getId());
				List<CellPhone> userCellPhones = dynamoDBService.findCellphonesByUserId(userId);
				CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, userId, collaborateFolder.getOwnerId());
				if (!collaborateAuth.isOwner() && !collaborateAuth.isMember(members, userCellPhones)) {
					result.setSmsStatusCodeForEachCellphones(HttpStatus.CANT_ACCESS_COLLABORATE.getCode());
					continue;
				}

				// is registered
				SSOFindUserIdByCellphoneReq ssoFindUserIdByCellphoneReq = new SSOFindUserIdByCellphoneReq(member.getCellphones());
				SSOFindUserIdByCellphone userIds = ssoService.findUserIdByCellophones(ssoFindUserIdByCellphoneReq);
				if (userIds.getUsers().isEmpty() == false) {
					result.setSmsStatusCodeForEachCellphones(HttpStatus.COLLABORATE_MEMBER_REGISTERED.getCode());
					continue;
				}

				// send
				SSOUserInfoResponse ownerUserInfo = ssoService.getUserInfoByToken(requestBase);
				String ownerUserName = ownerUserInfo.getName();
				String message = String.format(collaborateInviteSMSTemplate, ownerUserName, installDownloadUrl);

				for (CellPhone cellPhone : result.getCellphones()) {
					if (cellPhone.getSmsStatusCode() != null) {
						continue;
					} else if (StringUtils.isNullOrEmpty(cellPhone.getCellphone()) || StringUtils.isNullOrEmpty(cellPhone.getCountryCode())) {
						cellPhone.setSmsStatusCode(HttpStatus.ERROR_IN_PARAMETERS.getCode());
						continue;
					}
					SendSMSResponse smsResponse = ssoService.sendSMS(new SendSMSRequest(requestBase.getToken(), cellPhone.getCellphone(), cellPhone.getCountryCode(), message));
					String sid = "";
					String status = "error";
					if (response != null) {
						sid = smsResponse.getSid();
						status = smsResponse.getStatus();
					}
					collaborateDao.updateInviteProcessingStatus(member.getMemberId(), cellPhone.toString(), InviteType.CELLPHONE, sid, status);
					if (status.equals("error")) {
						cellPhone.setSmsStatusCode(HttpStatus.INTERNAL_SERVER_ERROR.getCode());
					} else {
						cellPhone.setSmsStatusCode(HttpStatus.OK.getCode());
					}
				}
				collaborateDao.updateCollaborateMemberInvitedBySMS(item.getMemberId());
			} catch (Exception e) {
				logger.error("inviteMemberBySMS error", e);
			}
		}
		return response;
	}

	@Override
	@Transactional
	public CreateCollaborateResponse transformToShared(CreateCollaborateRequest createCollaborateRequest) throws MetadataException {
		PathInfo pathInfo = pathFactory.parsePathInfo(createCollaborateRequest, false, true);
		Metadata originMetadata = metadataDao.getMetadataByPath(pathInfo.getFullOriginPath(), "", "");
		if (originMetadata == null || !originMetadata.isFolder()) {
			throw new MetadataException(HttpStatus.CANT_CREATE_COLLABORATE);
		}
		// check upper level and sub type
		checkUpperType(originMetadata, pathInfo.getUserId());
		List<Metadata> subMetadatas = metadataFactory.getAllSubFoldersAndFiles(originMetadata);
		subMetadatas.add(originMetadata);
		for (Metadata metadata : subMetadatas) {
			if (!metadata.getType().equals(MetadataType.NORMAL.toString())
					&& !metadata.getType().equals(MetadataType.COMMUNICATIONCOMMON.toString())) {
				throw new MetadataException(HttpStatus.CANT_CREATE_COLLABORATE);
			}else if (!StringUtils.isNullOrEmpty(metadata.getSyncRootId())) {
				throw new MetadataException(HttpStatus.LOWER_LEVEL_OF_TARGET_ALREADY_SYNC);
			}else if (!StringUtils.isNullOrEmpty(metadata.getSharedRootId())) {
				throw new MetadataException(HttpStatus.CANT_CREATE_COLLABORATE);
			}
			metadata.setModifiedAt(DateUtils.nowUTCTimestamp());
			metadata.setSharedRootId(originMetadata.getId());
		}
		// transform to share
		collaborateDao.createCollaborate(originMetadata.getId());
		metadataDao.batchUpdateMetadata(subMetadatas);
		metadataDao.updateMetadataType(originMetadata.getId(), MetadataType.SHARE);
		CountManageModel countManageModel = new CountManageModel();
		countManageModel.addFolder(originMetadata.getId(), 0, 0, 0, 1, 0,0);
		countService.updateAllParentCount(countManageModel);
		subMetadatas = null;
		// response
		String displayName = ssoService.findDisplayNameByUserId(createCollaborateRequest.getUserId());
		CreateCollaborateResponse collaborateResponse = new CreateCollaborateResponse();
		collaborateResponse.setOwnerId(createCollaborateRequest.getUserId());
		collaborateResponse.setOwner(displayName);
		collaborateResponse.setName(originMetadata.getOriginName());
		collaborateResponse.setSharedRootId(originMetadata.getSharedRootId());
		// send XMPP
		pathInfo.setSharedRootId(originMetadata.getSharedRootId());
		xmppService.sendToAllDevices(SystemEvent.UPDATE_COLLABORATE, pathInfo, null, true, false);
		return collaborateResponse;
	}

	@Override
	public boolean canAccessCollaborate(String sharedRootId, String ownerId, String userId,String path) {
		return checkCollaborate(sharedRootId,ownerId,userId,path).isCanAccessCollaborate();
	}
	private CollaborateMap checkCollaborate(String sharedRootId, String ownerId, String userId,String path){
		Metadata collaborateFolder = null;
		if (StringUtils.isNullOrEmpty(sharedRootId)) {
			Metadata metadata = metadataDao.getMetadataByPath(path, "", "");
			if (metadata == null) {
				throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
			}
			collaborateFolder = metadataDao.getMetadata(metadata.getSharedRootId());
		}else{
			collaborateFolder = metadataDao.getMetadata(sharedRootId);
		}
		CollaborateAuth collaborateAuth = new CollaborateAuth(collaborateFolder, userId, ownerId);
		CollaborateMap collaborateMap = new CollaborateMap();
		if (collaborateAuth.isOwner()) {
			collaborateMap.setCollaborateFolder(collaborateFolder);
			List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(collaborateFolder.getSharedRootId());
			collaborateMap.setCanAccessCollaborate(true);
			collaborateMap.setMembers(members);
		} else {
			List<CollaborateMember> members = collaborateDao.getCollaborateMembersByMetadataIndexId(collaborateFolder.getSharedRootId());
			List<CellPhone> userCellPhones = dynamoDBService.findCellphonesByUserId(userId);
			if (collaborateAuth.isMember(members, userCellPhones)) {
				collaborateMap.setCollaborateFolder(collaborateFolder);
				collaborateMap.setCanAccessCollaborate(true);
				collaborateMap.setMembers(members);
			} else {
				throw new MetadataException(HttpStatus.CANT_ACCESS_COLLABORATE);
			}
		}
		return collaborateMap;
	}

	private CollaborateMap getCollaborateMapByMemberId(long memberId) {
		CollaborateMember member = collaborateDao.getCollaborateMemberByMemberId(memberId);
		if (member == null) {
			throw new MetadataException(HttpStatus.COLLABORATE_MEMBER_NOT_EXIST);
		}
		SSOFindUserIdByCellphone memberInfo = ssoService.findUserIdByCellophones(new SSOFindUserIdByCellphoneReq(member.getCellphones()));
		if (memberInfo.checkUsersExists()) {
			member.setExists(true);
		}
		Metadata collaborateFolder = metadataDao.getMetadata(member.getMetadataIndexId());
		if (collaborateFolder == null) {
			throw new MetadataException(HttpStatus.FOLDER_NOT_EXIST);
		}
		CollaborateMap collaborateMap = new CollaborateMap();
		collaborateMap.setCollaborateFolder(collaborateFolder);
		collaborateMap.setMember(member);
		return collaborateMap;
	}

	

	private CollaborateMember createCollaborateMember(String userId, String fullPath, Metadata collaborateFolder, CollaborateMember member) {
		// check collaborateMember registered or not
		String cellphones = CellphonesUtils.composeCellphones(member.getCellphones());
		SSOFindUserIdByCellphone memberInfo = ssoService.findUserIdByCellophones(new SSOFindUserIdByCellphoneReq(member.getCellphones()));
		for (CellPhone user : memberInfo.getUsers()) {
			if (collaborateFolder.getOwnerId().equals(user.getUserId())) {
				throw new MetadataException(HttpStatus.CANT_CREATE_COLLABORATE_MEMBER);
			} else if (!StringUtils.isNullOrEmpty(user.getUserId())) {
				member.setExists(true);
			}
		}
		// check member exists or not
		CollaborateMember originMember = null;
		// no cellphone always create
		if (cellphones != null && !cellphones.equals("") && !cellphones.equals(",")){
			originMember = collaborateDao.getCollaborateMember(collaborateFolder.getId(), cellphones);
	    }
		if (originMember == null) {
			// create new collaborate member
			member.setMemberId(collaborateDao.createCollaborateMember(member, collaborateFolder.getId()));
			// send activity history
			sendActivityHistory(collaborateFolder,fullPath, userId, ChangeReason.INVITE_COLLABORATE_MEMBER);
			sendActivityHistory(collaborateFolder,fullPath, userId, ChangeReason.CREATE_COLLABORATE_MEMBER);
		} else {
			// update collaborate member
			if (originMember.getAccepted() != null && !originMember.getAccepted()) {
				member.setAccepted(null);
			} else {
				member.setAccepted(originMember.getAccepted());
			}
			collaborateDao.updateCollaborateMember(member, collaborateFolder.getId());
			// send activity history
			sendActivityHistory(collaborateFolder, fullPath,userId, ChangeReason.UPDATE_COLLABORATE_MEMBER);
		}
		return member;

	}

	private void sendActivityHistory(Metadata collaborateFolder,String fullPath, String userId, ChangeReason changeReason) {
		// send dynamoDB history
		String sharedUpperPath = metadataFactory.getSharedUpperPath(collaborateFolder);
		dynamoDBService.createActivityHistory(fullPath, "", sharedUpperPath,
				"", collaborateFolder.getId(), userId,
				DateUtils.nowUTCDateTime(), changeReason.toString(),
				collaborateFolder.getId(),
				ActivityHistoryLevel.NORMAL.toString());
	}

	private void checkUpperType(Metadata folder, String userId) {
		// check upper level type
		List<Metadata> upperMetadatas = metadataFactory.getAllUpperLevelMetadatas(folder);
		upperMetadatas.add(folder);
		for (Metadata metadata : upperMetadatas) {
			if (metadata.getOriginName().equals(userId)) {
				continue;
			}
			if (!metadata.getType().equals(MetadataType.NORMAL.toString())
					&& !metadata.getType().equals(MetadataType.COMMUNICATIONCOMMON.toString())) {
				throw new MetadataException(HttpStatus.CANT_CREATE_COLLABORATE);
			}else if (!StringUtils.isNullOrEmpty(metadata.getSyncRootId())) {
				throw new MetadataException(HttpStatus.UPPER_LEVEL_OF_SOURCE_ALREADY_SYNC);
			}else if (!StringUtils.isNullOrEmpty(metadata.getSharedRootId())) {
				throw new MetadataException(HttpStatus.CANT_CREATE_COLLABORATE);
			}
		}
	}

	/**
	 * It is highly recommend that prepare all members in one call
	 */
	@Override
	public void updateMemberDisplayNameFromSSO(List<CollaborateMember> members) {
		List<CellPhone> cellphones = new ArrayList<CellPhone>();
		for(CollaborateMember member : members) {
			cellphones.addAll(member.getCellphones());
		}
		
		Map<String, String> cellphoneDisplayNameMap = findCellphoneDisplayNameMap(cellphones);
		
		for(CollaborateMember member : members) {
			for(CellPhone cellphone : member.getCellphones()) {
				String cellphoneString = cellphone.toString(); 
				if(cellphoneDisplayNameMap.containsKey(cellphoneString)) {
					member.setDisplayName(cellphoneDisplayNameMap.get(cellphoneString));
					member.setRegistered(true);
					member.setExists(true);
					break;
				}
			}
		}
	}
	
	/**
	 * It is highly recommend that prepare all cellphones in one call
	 */
	@Override
	public Map<String, String> findCellphoneDisplayNameMap(List<CellPhone> cellphones) {
		Map<String, String> result = new HashMap<String, String>();
		
		Map<String, String> cellphoneUserIdMap = ssoService.findCellphoneUserIdMap(cellphones);
		Map<String, String> userIdDisplayNameMap = ssoService.findUserIdDisplayNameMap(new ArrayList<String>(cellphoneUserIdMap.values()));
		
		for(String cellphone: cellphoneUserIdMap.keySet()) {
			String mappedUserId = cellphoneUserIdMap.get(cellphone);
			if(!StringUtils.isNullOrEmpty(mappedUserId)) {
				if(userIdDisplayNameMap.containsKey(mappedUserId)) {
					String mappedDisplayName = userIdDisplayNameMap.get(mappedUserId);
					if(!StringUtils.isNullOrEmpty(mappedDisplayName)) {
						result.put(cellphone, mappedDisplayName);
					}
				}
			}
		}
		
		return result;
	}

	private void inviteUnregisteredMembers(String ownerToken, String inviterUserId, List<CollaborateMember> members) {
		if (members == null || members.isEmpty()) {
			return;
		}
		String inviterUserName = ssoService.findDisplayNameByUserId(inviterUserId);
		String message = String.format(collaborateInviteSMSTemplate, inviterUserName, installDownloadUrl);

		for (CollaborateMember member : members) {
			// is registered
			if (member.isExists()) {
				member.updateSmsStatusCodeForEachCellphones(HttpStatus.COLLABORATE_MEMBER_REGISTERED.getCode());
				member.updateMailStatusCodeForEachMails(HttpStatus.COLLABORATE_MEMBER_REGISTERED.getCode());
				continue;
			}
			// mail
			member.setInvitedByMail(false);
			if (member.getMails() != null && member.getMails().isEmpty() == false) {
				String displayInviteCellphones = "";
				if (member.getCellphones() != null && !member.getCellphones().isEmpty() && !member.getCellphones().get(0).getCellphone().equals("")) {
					displayInviteCellphones = CellphonesUtils.toDisplayCellphone(member.getCellphones());
				}
				String body = String.format(collaborateInviteMailTemplate, gatewayExternalUrl, inviterUserName, displayInviteCellphones, webPortal);
				String subject = String.format(collaborateInviteMailSubjectTemplate, inviterUserName);
				for (Mail mail : member.getMails()) {
					if (emailService.sendEmail(mail.getMail(), subject, body)) {
						mail.setMailStatusCode(HttpStatus.OK.getCode());
						collaborateDao.updateInviteProcessingStatus(member.getMemberId(), mail.getMail(), InviteType.EMAIL, "", "success");
					} else {
						mail.setMailStatusCode(HttpStatus.INTERNAL_SERVER_ERROR.getCode());
						collaborateDao.updateInviteProcessingStatus(member.getMemberId(), mail.getMail(), InviteType.EMAIL, "", "error");
					}
				}
				collaborateDao.updateCollaborateMemberInvitedByMail(member.getMemberId());
				member.setInvitedByMail(true);
			}
			// cellphone
			member.setInvitedBySMS(false);
			if (member.getCellphones() != null && member.getCellphones().isEmpty() == false) {
				for (CellPhone cellPhone : member.getCellphones()) {
					if (cellPhone.getCellphone() != null && !cellPhone.getCellphone().equals("")) {
						SendSMSRequest sendSMSRequest = new SendSMSRequest(ownerToken, cellPhone.getCellphone(), cellPhone.getCountryCode(), message);
						SendSMSResponse response = ssoService.sendSMS(sendSMSRequest);
						String sid = "";
						String status = "error";
						if (response != null) {
							sid = response.getSid();
							status = response.getStatus();
						}
						collaborateDao.updateInviteProcessingStatus(member.getMemberId(), cellPhone.toString(), InviteType.CELLPHONE, sid, status);
						if (status.equals("error")) {
							cellPhone.setSmsStatusCode(HttpStatus.INTERNAL_SERVER_ERROR.getCode());
						} else {
							cellPhone.setSmsStatusCode(HttpStatus.OK.getCode());
						}
					}
				}
				collaborateDao.updateCollaborateMemberInvitedBySMS(member.getMemberId());
				member.setInvitedBySMS(true);
			}
		}
	}

}