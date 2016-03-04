package dao;

import java.util.List;

import model.CollaborateMember;
import model.InviteProcessingStatus;
import model.Metadata;
import model.SourceTargetMap;
import constants.InviteType;

public interface CollaborateDao {
	public void createCollaborate(String metadataId);

	public CollaborateMember getCollaborateMember(String metadataIndexId,
			String cellphones);

	public CollaborateMember getCollaborateMemberByFragmentCellphone(
			String metadataIndexId, String cellphone);

	public List<CollaborateMember> getCollaborateMembersByMetadataIndexId(
			String metadataIndexId);

	public long createCollaborateMember(CollaborateMember collaborateMember,
			String metadataIndexId);

	public void updateCollaborateMember(CollaborateMember collaborateMember,
			String metadataIndexId);

	public void batchCreateCollaboratebyId(List<String> idList);

	public void updateMemberAccepted(Boolean accepted, String metadataIndexId,
			String cellphones);

	public CollaborateMember getCollaborateMemberByMemberId(long memberId);

	public void updateCollaborateMemberInvitedByMail(long memberId);

	public void updateCollaborateMemberInvitedBySMS(long memberId);

	public void updateCollaborateMemberInvitedBySMSAndMail(long memberId);

	public boolean updateCollaborateMemberPhoto(long memberId,
			String photoBlockId, int photoSize);

	public void deleteCollaborateMember(long memberId);

	public void deleteCollaborate(String metadataIndexId);

	public void deleteCollaborate4DB(String metadataIndexId);

	public List<CollaborateMember> getCollaborateMembersByCellphones(
			String cellphones, boolean keepEachSameUserMember, boolean includeInvisibleFolder, boolean includeAcceptedOnly);

	public void cleanSharedRootId(String shareRootId);

	public void batchUpdateMetaIdxId(List<SourceTargetMap> list);

	public void batchUpdateCollaborate(List<String> list);
	
	public void batchCreateCollaborate(List<Metadata> idList);
	
	public void updateInviteProcessingStatus(long memberId, String inviteTo, InviteType type, String smsSid, String status);

	public List<InviteProcessingStatus> getInviteProcessingStatus(long memberId);
}
