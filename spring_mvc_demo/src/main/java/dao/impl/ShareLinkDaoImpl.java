package dao.impl;

import java.sql.Types;

import model.ShareLink;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.BeanPropertySqlParameterSource;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;

import dao.ShareLinkDao;

@Repository
public class ShareLinkDaoImpl implements ShareLinkDao {

	@Value("${sql.create_share_link}")
	private String createShareLink;
	@Value("${sql.get_share_link_info}")
	private String getShareLinkInfo;
	@Value("${sql.get_share_link}")
	private String getShareLink;
	@Value("${sql.delete_share_link}")
	private String deleteShareLink;
	@Value("${sql.update_share_link}")
	private String updateShareLink;

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;

	@Override
	public ShareLink createShareLink(ShareLink shareLink) {

		jdbcTemplate.update(createShareLink,
				new BeanPropertySqlParameterSource(shareLink));
		return shareLink;
	}

	@Override
	public String getShareLink(String metadataIndexId) {
		String link = null;
		try {
			link = jdbcTemplate.queryForObject(getShareLink,
					new MapSqlParameterSource().addValue("metadataIndexId",
							metadataIndexId, Types.VARCHAR), String.class);
		} catch (EmptyResultDataAccessException e) {
		}
		return link;
	}

	@Override
	public ShareLink getShareLinkInfo(String link) {
		ShareLink shareLink = null;
		try {
			shareLink = jdbcTemplate.queryForObject(getShareLinkInfo,
					new MapSqlParameterSource().addValue("link", link,
							Types.VARCHAR), new BeanPropertyRowMapper<ShareLink>(ShareLink.class));
		} catch (EmptyResultDataAccessException e) {
		}
		return shareLink;
	}

	@Override
	public void deleteShareLink(String metadataIndexId) {
		jdbcTemplate.update(deleteShareLink, new MapSqlParameterSource()
				.addValue("metadataIndexId", metadataIndexId, Types.VARCHAR));
	}


	@Override
	public ShareLink updateShareLink(ShareLink shareLink) {
		jdbcTemplate.update(updateShareLink,
				new BeanPropertySqlParameterSource(shareLink));
		return shareLink;
	}

}
