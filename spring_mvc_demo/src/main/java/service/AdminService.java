package service;

import model.request.GetClientVersionRequest;
import model.request.SendAllMetadataToMQRequest;
import model.request.UpdateClientVersionRequest;
import model.response.ClientVersionResponse;
import exception.MetadataException;

public interface AdminService {

	public Boolean sendAllMetadataToMQ(SendAllMetadataToMQRequest sendAllMetadataToMQRequest) throws MetadataException;

	public ClientVersionResponse getClientVersion(GetClientVersionRequest getClientVersionRequest) throws MetadataException;

	public void updateClientVersion(UpdateClientVersionRequest updateClientVersionRequest) throws MetadataException;
}