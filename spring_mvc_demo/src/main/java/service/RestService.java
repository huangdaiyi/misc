package service;

import org.springframework.http.HttpHeaders;

public interface RestService {

	public <T> String sendPostRequest(String url, HttpHeaders headers, T requestBody);

	public <T> String sendPutRequest(String url, HttpHeaders headers, T requestBody);
	
	public <T> String sendDeleteRequest(String url, HttpHeaders headers, T requestBody);

	public String sendGetRequestPartialContent(String url, HttpHeaders headers);

	public <T> T sendPostRequestForObj(String url, HttpHeaders headers, Object request, Class<T> responseType);

}
