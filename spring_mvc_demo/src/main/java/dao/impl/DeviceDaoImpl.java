package dao.impl;

import java.sql.Types;
import java.util.List;

import model.Device;
import model.MobileFolders;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import utils.StringUtils;
import dao.DeviceDao;

@Repository
public class DeviceDaoImpl implements DeviceDao {

	@Value("${sql.update_device_setting}")
	private String updateDeviceSetting;

	@Value("${sql.find_device}")
	private String findDeviceSql;

	@Value("${sql.find_all_device}")
	private String findAllDeviceSql;

	@Value("${sql.create_device}")
	private String createDeviceSql;

	@Value("${sql.find_last_insert_id}")
	private String findLastInsertIdSql;

	@Value("${sql.delete_device}")
	private String deleteDeviceSql;
	
	@Value("${sql.find_mobile_folders}")
	private String findMobileFoldersSql;
	
	@Value("${sql.create_mobile_folders}")
	private String createMobileFolderSql;
	
	@Value("${sql.find_device_by_deviceid}")
	private String findDeviceByDeviceId;

	@Autowired
	private NamedParameterJdbcTemplate jdbcTemplate;
	
	// bean row mapper
	private BeanPropertyRowMapper<Device> deviceBeanRowMapper = new BeanPropertyRowMapper<Device>(Device.class);

	@Override
	public void  update(String userId, String deviceUid, String settings) {
		jdbcTemplate.update(updateDeviceSetting, new MapSqlParameterSource()
				.addValue("userId", userId)
				.addValue("deviceUid", deviceUid)
				.addValue("settings", settings));
	}

	@Override
	public Device getDevice(String userId, String deviceUid) {
		if (StringUtils.isNullOrEmpty(userId) || StringUtils.isNullOrEmpty(deviceUid)) {
			return null;
		}
		try {
			return jdbcTemplate.queryForObject(
					findDeviceSql,
					new MapSqlParameterSource().addValue("deviceUid",
							deviceUid, Types.VARCHAR).addValue("userId",
							userId, Types.VARCHAR), deviceBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return null;
	}

	@Override
	public List<Device> getDevices(String user) {
		return jdbcTemplate.query(findAllDeviceSql, new MapSqlParameterSource()
				.addValue("userId", user, Types.VARCHAR), deviceBeanRowMapper);
	}

	@Override
	@Transactional
	public Device createDevice(Device device) {
		int effect = jdbcTemplate.update(createDeviceSql,
				new MapSqlParameterSource()
						.addValue("userId", device.getUserId(), Types.VARCHAR)
						.addValue("deviceId", device.getDeviceId(), Types.VARCHAR)
						.addValue("lastEditUser", device.getLastEditUser(), Types.VARCHAR)
						.addValue("lastEditTime", device.getLastEditTime(), Types.BIGINT)
						.addValue("deviceUid", device.getDeviceUid(), Types.VARCHAR)
						.addValue("settings", device.getSettings(), Types.VARCHAR));
		if (effect == 1) {
			device.setId(jdbcTemplate.queryForObject(findLastInsertIdSql,
					(MapSqlParameterSource) null, Integer.class));
		}
		return device;
	}

	@Override
	public void deleteDevice(int id) {
		jdbcTemplate.update(deleteDeviceSql, new MapSqlParameterSource().addValue("id", id, Types.INTEGER));
	}

	@Override
	public MobileFolders getMobileFolders(String userId, String fullSourcePath) {
		MobileFolders mobileFolders = null;
		try {
			mobileFolders = jdbcTemplate.queryForObject(
					findMobileFoldersSql,
					new MapSqlParameterSource().addValue("fullSourcePath",
							fullSourcePath, Types.VARCHAR).addValue("userId",
							userId, Types.VARCHAR),MobileFolders.getMapper());
		} catch (EmptyResultDataAccessException e) {
		}
		return mobileFolders;
	}

	@Override
	public void createMobileFolders(String userId, String fullSourcePath,
			String deviceUid) {
		jdbcTemplate.update(createMobileFolderSql,
				new MapSqlParameterSource()
						.addValue("userId", userId, Types.VARCHAR)
						.addValue("deviceUid", deviceUid, Types.VARCHAR)
						.addValue("fullSourcePath", fullSourcePath, Types.VARCHAR)
						.addValue("status", "sync", Types.VARCHAR));
		
	}

	@Override
	public Device getDeviceByDeviceId(String userId, String deviceId) {
		Device device = null;
		try {
			device = jdbcTemplate.queryForObject(
					findDeviceByDeviceId,
					new MapSqlParameterSource().addValue("deviceId",
							deviceId, Types.VARCHAR).addValue("userId",
							userId, Types.VARCHAR), deviceBeanRowMapper);
		} catch (EmptyResultDataAccessException e) {
		}
		return device;
	}
	
}
