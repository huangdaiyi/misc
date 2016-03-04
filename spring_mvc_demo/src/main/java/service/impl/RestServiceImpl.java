package service.impl;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.Proxy.Type;
import java.net.URI;

import javax.annotation.PostConstruct;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import service.RestService;

@Service
public class RestServiceImpl implements RestService {
	private static final Logger logger = LogManager.getLogger(RestServiceImpl.class);

	@Value("${http.proxy.enabled}")
	private boolean proxyEnabled;
	@Value("${http.proxy.host}")
	private String proxyHost;
	@Value("${http.proxy.port}")
	private int proxyPort;

	@Autowired
	private RestTemplate restTemplate;

	@PostConstruct
	private void init() {
		if (proxyEnabled) {
			SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
			Proxy proxy = new Proxy(Type.HTTP, new InetSocketAddress(proxyHost, proxyPort));
			requestFactory.setProxy(proxy);
			restTemplate.setRequestFactory(requestFactory);
		}
	}

	@Override
	public <T> String sendPostRequest(String url, HttpHeaders headers, T requestBody) {
		String response = null;
		try {
			HttpEntity<T> entity = new HttpEntity<T>(requestBody, headers);
			ResponseEntity<String> result = restTemplate.exchange(url, HttpMethod.POST, entity, String.class);
			if (result.getStatusCode() == HttpStatus.OK) {
				response = result.getBody();
			}
		} catch (Exception e) {
			logger.error("RestServiceImpl(Exception):{}", e);
		}
		return response;
	}

	@Override
	public <T> String sendPutRequest(String url, HttpHeaders headers, T requestBody) {
		String response = null;
		try {
			HttpEntity<T> entity = new HttpEntity<T>(requestBody, headers);
			ResponseEntity<String> result = restTemplate.exchange(url, HttpMethod.PUT, entity, String.class);
			if (result.getStatusCode() == HttpStatus.OK) {
				response = result.getBody();
			}
		} catch (Exception e) {
			logger.error("RestServiceImpl(Exception):{}", e);
		}
		return response;
	}
	
	@Override
	public <T> String sendDeleteRequest(String url, HttpHeaders headers, T requestBody) {
		String response = null;
		try {
			HttpEntity<T> entity = new HttpEntity<T>(requestBody, headers);
			ResponseEntity<String> result = restTemplate.exchange(url, HttpMethod.DELETE, entity, String.class);
			if (result.getStatusCode() == HttpStatus.OK) {
				response = result.getBody();
			}
		} catch (Exception e) {
			logger.error("RestServiceImpl(Exception):{}", e);
		}
		return response;
	}

	@Override
	public String sendGetRequestPartialContent(String url, HttpHeaders headers) {
		String response = null;
		try {
			HttpEntity<String> entity = new HttpEntity<String>(null, headers);
			ResponseEntity<String> result = restTemplate.exchange(url, HttpMethod.GET, entity, String.class);
			if (result.getStatusCode() == HttpStatus.PARTIAL_CONTENT) {
				response = result.getBody();
			}
		} catch (Exception e) {
			logger.error("RestServiceImpl(Exception):{}", e);
		}
		return response;
	}

	@Override
	public <T> T sendPostRequestForObj(String url, HttpHeaders headers, Object request, Class<T> responseType) {
		T response = null;
		try {
			HttpEntity<?> entity = new HttpEntity<Object>(request, headers);
			URI uri = new URI(url);
			ResponseEntity<T> result = restTemplate.postForEntity(uri, entity, responseType);
			if (result.getStatusCode() == HttpStatus.OK) {
				response = result.getBody();
			}
		} catch (Exception e) {
			logger.error("RestServiceImpl(Exception):{}", e);
		}
		return response;
	}

}
