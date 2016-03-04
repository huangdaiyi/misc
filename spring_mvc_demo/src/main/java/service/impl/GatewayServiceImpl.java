package service.impl;

import java.util.HashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;

import service.GatewayService;
import service.RestService;
import constants.HttpStatus;
import exception.MetadataException;

@Service
public class GatewayServiceImpl implements GatewayService {

	@Value("${gateway.url}")
	private String gatewayUrl;
	@Value("${gateway.tokenName}")
	private String tokenName;
	@Value("${gateway.keyName}")
	private String keyName;
	@Value("${gateway.oldKeyName}")
	private String oldKeyName;
	@Value("${gateway.checkPasswordUrl}")
	private String checkPasswordUrl;
	@Value("${gateway.encryptFileUrl}")
	private String encryptFileUrl;
	@Value("${gateway.decryptFileUrl}")
	private String decryptFileUrl;
	
	@Autowired
	private RestService restService;
	
	@Override
	public void checkPassword(String token, String password, String blockId) {
		String url = gatewayUrl.concat(checkPasswordUrl);
		HttpHeaders headers = new HttpHeaders();
		headers.add(tokenName, token);
		headers.add(keyName, password);
		headers.setContentType(MediaType.APPLICATION_JSON);
		HashMap<String, String> body = new HashMap<String, String>();
		body.put("sha256", blockId);
		String responseBody = restService.sendPostRequest(url, headers, body);
		if (responseBody == null) {
			throw new MetadataException(HttpStatus.PASSWORD_INCORRECT);
		}
	}
	
	@Override
	public String encryptFile(String token, String password, String blockId) {
		String url = gatewayUrl.concat(encryptFileUrl).concat(blockId);
		HttpHeaders headers = new HttpHeaders();
		headers.add(tokenName, token);
		headers.add(keyName, password);
		headers.add(oldKeyName, "");
		headers.setContentType(MediaType.TEXT_PLAIN);
		String newBlockId = restService.sendPutRequest(url, headers, null);
		if (newBlockId == null) {
			throw new MetadataException(HttpStatus.GATEWAY_FAILED);
		} 
		return newBlockId;
	}

	@Override
	public String decryptFile(String token, String password, String blockId) {
		String url = gatewayUrl.concat(decryptFileUrl).concat(blockId);
		HttpHeaders headers = new HttpHeaders();
		headers.add(tokenName, token);
		headers.add(keyName, "");
		headers.add(oldKeyName, password);
		headers.setContentType(MediaType.TEXT_PLAIN);
		String newBlockId =  restService.sendPutRequest(url, headers, null);
		if (newBlockId == null) {
			throw new MetadataException(HttpStatus.GATEWAY_FAILED);
		} 
		return newBlockId;
	}

}
