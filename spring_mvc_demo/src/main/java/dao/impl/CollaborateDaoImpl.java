package dao.impl;

import java.sql.Types;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import model.CollaborateMember;
import model.InviteProcessingStatus;
import model.Metadata;
import model.SourceTargetMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.jdbc.core.namedparam.SqlParameterSourceUtils;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

import utils.StringUtils;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import constants.InviteType;
import dao.CollaborateDao;

@Repository
public class CollaborateDaoImpl implements CollaborateDao {

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	@Value("${sql.create_collaborate_setting}")
	private String createCollaborateSql;
	@Value("${sql.get_collaborate_metadata}")
	private String getCollaborateSql;
	@Value("${sql.get_collaborate_member_by_completed_cellphones}")
	private String getCollaborateMemberByCompletedCellphonesSql;
	@Value("${sql.get_collaborate_members}")
	private String getCollaborateMembersSql;
	@Value("${sql.get_collaborate_member_by_ambiguous_full_cellphones}")
	private String getCollaborateMemberByAmbiguousFullCellphonesSql;
	@Value("${sql.create_collaborate_member}")
	private String createCollaborateMemberSql;
	@Value("${sql.update_collaborate_member}")
	private String updateCollaborateMemberSql;
	@Value("${sql.batch_create_collaborate}")
	private String batchCreateCollaboratSql;
	@Value("${sql.update_member_accepted}")
	private String updateMemberAcceptedSql;
	@Value("${sql.get_metadata_info_by_member_id}")
	private String getCollaborateMemberByMemberIdSql;
	@Value("${sql.update_collaborate_member_invited_by_mail}")
	private String updateCollaborateMemberInvitedByMailSql;
	@Value("${sql.update_collaborate_member_invited_by_sms}")
	private String updateCollaborateMemberInvitedBySMSSql;
	@Value("${sql.update_collaborate_member_invited_by_sms_and_mail}")
	private String updateCollaborateMemberInvitedBySMSAndMailSql;
	@Value("${sql.delete_collaborate_member}")
	private String deleteCollaborateMemberSql;
	@Value("${sql.delete_collaborate}")
	private String deleteCollaborateSql;
	@Value("${sql.delete_collaborate_for_db}")
	private String deleteCollaborate4DBSql;
	@Value("${sql.update_collaborate_member_photo}")
	private String updateCollaborateMemberPhotoSql;
	@Value("${sql.get_collaborate_members_by_fragment_cellphone}")
	private String getCollaborateMembersByFragmentCellphoneSql;
	@Value("${sql.update_shared_root_id_to_null}")
	private String updateSharedRootIdToNullSql;
	@Value("${sql.update_collaborate_member_metadata_index_id}")
	private String updateCollaborateMetaDataIndexIdSql;
	@Value("${sql.batch_update_collaborate}")
	private String batchUpdateCollaborate;
	@Value("${sql.update_invite_ptocessing_status}")
	private String updateInviteProcessingStatusSql;
	@Value("${sql.get_invite_ptocessing_statuses}")
	private String getInviteProcessingStatusesSql;

	// bean row mapper
	private BeanPropertyRowMapper<CollaborateMember> collaborateMemberBeanRowMapper = new BeanPropertyRowMapper<CollaborateMember>(CollaborateMember.class);
	private BeanPropertyRowMapper<InviteProcessingStatus> inviteProcessingStatusBeanRowMapper = new BeanPropertyRowMapper<InviteProcessingStatus>(InviteProcessingStatus.class);

	@Override
	public void createCollaborate(String metadataId) {
		jdbcTemplate.update(createCollaborateSql, new MapSqlParameterSource()
				.addValue("metadataId", metadataId, Types.VARCHAR));
	}

	@Override
	public void batchUpdateCollaborate(List<String> list) {
		MapSqlParameterSource param = new MapSqlParameterSource();
		param.addValue("id", list);
		jdbcTemplate.update(batchUpdateCollaborate, param);
	}

	@Override
	public void batchUpdateMetaIdxId(List<SourceTargetMap> list) {
		SqlParameterSource[] params = SqlParameterSourceUtils.createBatch(list
				.toArray());
		jdbcTemplate.batchUpdate(updateCollaborateMetaDataIndexIdSql, params);
	}

	@Override
	public void batchCreateCollaborate(List<Metadata> idList) {
		SqlParameterSource[] params = SqlParameterSourceUtils
				.createBatch(idList.toArray());
		jdbcTemplate.batchUpdate(batchCreateCollaboratSql, params);
	}

	@Override
	public List<CollaborateMember> getCollaborateMembersByCellphones(String cellphones, final boolean keepEachSameUserMember, boolean includeInvisibleFolder, final boolean includeAcceptedOnly) {
		List<CollaborateMember> result = new ArrayList<CollaborateMember>();
		if (StringUtils.isNullOrEmpty(cellphones)) {
			return result;
		}
		for (String cellphone : cellphones.split("/")) {
			if (StringUtils.isNullOrEmpty(cellphone) || cellphone.equals(",")) {
				continue;
			}
			result.addAll(jdbcTemplate.query(getCollaborateMembersByFragmentCellphoneSql, new MapSqlParameterSource().addValue("cellphone", cellphone).addValue("includeInvisibleFolder", includeInvisibleFolder),
					collaborateMemberBeanRowMapper));
		}
		// filter duplicated member id
		return Lists.newArrayList(Iterables.filter(result, new Predicate<CollaborateMember>() {
			private List<Long> existsMemberIds = new ArrayList<Long>();
			private Set<String> ignoreMetadataIndexIds = new HashSet<String>();

			@Override
			public boolean apply(CollaborateMember member) {
				if (member == null || existsMemberIds.contains(member.getMemberId()) || (member.getAccepted() != null && member.getAccepted() == false) || (includeAcceptedOnly && member.getAccepted() == null)) {
					return false;
				}
				existsMemberIds.add(member.getMemberId());
				if (keepEachSameUserMember == false) {
					if (ignoreMetadataIndexIds.contains(member.getMetadataIndexId())) {
						return false;
					}
					ignoreMetadataIndexIds.add(member.getMetadataIndexId());
				}
				return true;
			}
		}));
	}

	@Override
	public CollaborateMember getCollaborateMember(String metadataIndexId,
			String cellphones) {
		CollaborateMember collaborateMember = null;
		try {
			collaborateMember = jdbcTemplate.queryForObject(
					getCollaborateMemberByCompletedCellphonesSql,
					new MapSqlParameterSource().addValue("metadataIndexId",
							metadataIndexId, Types.VARCHAR).addValue(
							"cellphones", cellphones, Types.VARCHAR),
							collaborateMemberBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return collaborateMember;
	}

	@Override
	public List<CollaborateMember> getCollaborateMembersByMetadataIndexId(
			String metadataIndexId) {

		return jdbcTemplate.query(getCollaborateMembersSql,
				new MapSqlParameterSource().addValue("metadataIndexId",
						metadataIndexId, Types.VARCHAR), CollaborateMember
						.getMapper());
	}

	@Override
	public long createCollaborateMember(CollaborateMember collaborateMember,
			String metadataIndexId) {
		KeyHolder keyHolder = new GeneratedKeyHolder();
		jdbcTemplate
				.update(createCollaborateMemberSql,
						new MapSqlParameterSource()
								.addValue("metadataIndexId", metadataIndexId,
										Types.VARCHAR)
								.addValue("mails",
										collaborateMember.generateMails(),
										Types.VARCHAR)
								.addValue("nickname",
										collaborateMember.getNickname(),
										Types.VARCHAR)
								.addValue("cellphones", collaborateMember.generateCellphones(),
										Types.VARCHAR)
								.addValue("photoBlockId",
										collaborateMember.getPhotoBlockId(),
										Types.VARCHAR)
								.addValue("photoSize",
										collaborateMember.getPhotoSize(),
										Types.VARCHAR)
								.addValue("inviteMessage",
										collaborateMember.getInviteMessage(),
										Types.VARCHAR), keyHolder);

		return keyHolder.getKey().longValue();

	}

	@Override
	public void updateCollaborateMember(CollaborateMember collaborateMember,
			String metadataIndexId) {
		jdbcTemplate
				.update(updateCollaborateMemberSql,
						new MapSqlParameterSource()
								.addValue("metadataIndexId", metadataIndexId,
										Types.VARCHAR)
								.addValue("mails",
										collaborateMember.generateMails(),
										Types.VARCHAR)
								.addValue("nickname",
										collaborateMember.getNickname(),
										Types.VARCHAR)
								.addValue("cellphones", collaborateMember.generateCellphones(),
										Types.VARCHAR)
								.addValue("photoBlockId",
										collaborateMember.getPhotoBlockId(),
										Types.VARCHAR)
								.addValue("photoSize",
										collaborateMember.getPhotoSize(),
										Types.VARCHAR)
								.addValue("accepted",
										collaborateMember.getAccepted(),
										Types.BOOLEAN)
								.addValue("inviteMessage",
										collaborateMember.getInviteMessage(),
										Types.VARCHAR));

	}

	@Override
	public void updateMemberAccepted(Boolean accepted, String metadataIndexId,
			String cellphones) {
		jdbcTemplate.update(
				updateMemberAcceptedSql,
				new MapSqlParameterSource()
						.addValue("metadataIndexId", metadataIndexId,
								Types.VARCHAR)
						.addValue("tempCellphone", cellphones, Types.VARCHAR)
						.addValue("accepted", accepted, Types.BOOLEAN));
	}

	@Override
	public CollaborateMember getCollaborateMemberByMemberId(long memberId) {
		MapSqlParameterSource parameterSource = new MapSqlParameterSource();
		parameterSource.addValue("memberId", memberId);
		try {
			return jdbcTemplate.queryForObject(
					getCollaborateMemberByMemberIdSql, parameterSource,
					CollaborateMember.getMapper());
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public void updateCollaborateMemberInvitedByMail(long memberId) {
		jdbcTemplate.update(updateCollaborateMemberInvitedByMailSql,
				new MapSqlParameterSource().addValue("memberId", memberId,
						Types.INTEGER));
	}

	@Override
	public void deleteCollaborateMember(long memberId) {
		jdbcTemplate.update(deleteCollaborateMemberSql,
				new MapSqlParameterSource().addValue("memberId", memberId,
						Types.INTEGER));
	}

	@Override
	public void deleteCollaborate(String metadataIndexId) {
		jdbcTemplate.update(deleteCollaborateSql, new MapSqlParameterSource()
				.addValue("metadataIndexId", metadataIndexId, Types.VARCHAR));
	}

	@Override
	public void deleteCollaborate4DB(String metadataIndexId) {
		jdbcTemplate.update(deleteCollaborate4DBSql,
				new MapSqlParameterSource().addValue("metadataIndexId",
						metadataIndexId, Types.VARCHAR));
	}

	@Override
	public boolean updateCollaborateMemberPhoto(long memberId,
			String photoBlockId, int photoSize) {
		int i = jdbcTemplate.update(
				updateCollaborateMemberPhotoSql,
				new MapSqlParameterSource()
						.addValue("memberId", memberId, Types.INTEGER)
						.addValue("photoBlockId", photoBlockId, Types.VARCHAR)
						.addValue("photoSize", photoSize, Types.INTEGER));
		return i > 0 ? true : false;
	}

	@Override
	public CollaborateMember getCollaborateMemberByFragmentCellphone(
			String metadataIndexId, String cellphone) {
		CollaborateMember collaborateMember = null;
		try {
			collaborateMember = jdbcTemplate.queryForObject(
					getCollaborateMemberByAmbiguousFullCellphonesSql,
					new MapSqlParameterSource().addValue("metadataIndexId",
							metadataIndexId, Types.VARCHAR).addValue(
							"cellphones", cellphone, Types.VARCHAR),
							collaborateMemberBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return collaborateMember;
	}

	@Override
	public void updateCollaborateMemberInvitedBySMS(long memberId) {
		jdbcTemplate.update(updateCollaborateMemberInvitedBySMSSql,
				new MapSqlParameterSource().addValue("memberId", memberId,
						Types.INTEGER));

	}

	@Override
	public void updateCollaborateMemberInvitedBySMSAndMail(long memberId) {
		jdbcTemplate.update(updateCollaborateMemberInvitedBySMSAndMailSql,
				new MapSqlParameterSource().addValue("memberId", memberId,
						Types.INTEGER));
	}

	@Override
	public void cleanSharedRootId(String sharedRootId) {
		MapSqlParameterSource paramSource = new MapSqlParameterSource();
		paramSource.addValue("sharedRootId", sharedRootId);
		jdbcTemplate.update(updateSharedRootIdToNullSql, paramSource);
	}

	@Override
	public void batchCreateCollaboratebyId(List<String> idList) {
		List<Metadata> list = new ArrayList<Metadata>();
		for (String id : idList) {
			Metadata metadata = new Metadata();
			metadata.setId(id);
			list.add(metadata);
		}
		SqlParameterSource[] params = SqlParameterSourceUtils.createBatch(list
				.toArray());
		jdbcTemplate.batchUpdate(batchCreateCollaboratSql, params);
	}
	
	@Override
	public void updateInviteProcessingStatus(long memberId, String inviteTo, 
			InviteType type, String smsSid, String status) {
		jdbcTemplate.update(updateInviteProcessingStatusSql,
				new MapSqlParameterSource().addValue("memberId", memberId).addValue("inviteTo", inviteTo)
				.addValue("type", type.toString().toLowerCase()).addValue("smsSid", smsSid == null ? "":smsSid).addValue("status", status));

	}

	@Override
	public List<InviteProcessingStatus> getInviteProcessingStatus(long memberId) {
		return jdbcTemplate.query(getInviteProcessingStatusesSql, new MapSqlParameterSource("memberId", memberId), inviteProcessingStatusBeanRowMapper);
	}

}