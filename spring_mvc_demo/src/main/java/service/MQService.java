package service;

import model.BaseMetadata;
import model.Metadata;
import model.request.EncryptRequest;
import model.request.SendAllMetadataToMQRequest;
import model.request.UnzipRequest;
import model.request.ZipRequest;

public interface MQService {
	
	public void sendForStreaming(Metadata metadata, String token);
	
	public void sendForReader(Metadata metadata, String token);
	
	public void sendAddFullTextSearch(BaseMetadata metadata);

	public void sendForZip(ZipRequest zipRequest);

	public void sendForUnzip(UnzipRequest unzipRequest);

	public void sendForEncrypt(boolean isFolder, EncryptRequest encryptRequest);
	
	public void sendForDecrypt(EncryptRequest decryptRequest);

	public void sendForThumbnail(BaseMetadata metadata);
	
	public boolean sendAllMetadata(SendAllMetadataToMQRequest request);
	
}
