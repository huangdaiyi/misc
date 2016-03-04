package service.impl;

import model.request.GetClientVersionRequest;
import model.request.SendAllMetadataToMQRequest;
import model.request.UpdateClientVersionRequest;
import model.response.ClientVersionResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import service.AdminService;
import service.MQService;
import constants.HttpStatus;
import dao.ClientVersionDao;
import exception.MetadataException;

@Service
public class AdminServiceImpl implements AdminService {

	@Autowired
	private MQService mqservice;
	@Autowired
	private ClientVersionDao clientVersionDao;

	@Value("${stable.api.version}")
	private String stableApiVersion;

	@Override
	public Boolean sendAllMetadataToMQ(
			SendAllMetadataToMQRequest sendAllMetadataToMQRequest)
			throws MetadataException {
		return mqservice.sendAllMetadata(sendAllMetadataToMQRequest);
	}

	@Override
	public ClientVersionResponse getClientVersion(
			GetClientVersionRequest getClientVersionRequest)
			throws MetadataException {
		ClientVersionResponse response = clientVersionDao.getClientVersion(getClientVersionRequest.getPlatform());
		if (response == null) {
			throw new MetadataException(HttpStatus.FILE_NOT_EXIST);
		}
		response.setPlatform(null);
		response.setUpdateType(null);
		response.setStableApiVersion(stableApiVersion);
		return response;
	}

	@Override
	public void updateClientVersion(
			UpdateClientVersionRequest updateClientVersionRequest)
			throws MetadataException {
			clientVersionDao.updateClientVersion(updateClientVersionRequest.getPlatform(),
					updateClientVersionRequest.getVersion(), updateClientVersionRequest.getUpdateType(),
					updateClientVersionRequest.getDownloadUrl());
	}

}
