package factory.impl;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.amazonaws.ClientConfiguration;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.sqs.AmazonSQSClient;

import factory.AWSClientFactory;

@Component("sqsClientFactory")
public class SQSClientFactoryImpl implements AWSClientFactory<AmazonSQSClient> {

	protected AWSCredentials credentials;
	protected ClientConfiguration config;
	protected Region regionObject;

	@Value("${sqs.accessKey}")
	private String accessKey;
	@Value("${sqs.secretKey}")
	private String secretKey;
	@Value("${sqs.region}")
	private String region;
	@Value("${aws.maxConnections}")
	private int maxConnections;
	@Value("${aws.maxErrorRetry}")
	private int maxErrorRetry;
	@Value("${http.proxy.enabled}")
	private boolean proxyEnabled;
	@Value("${http.proxy.host}")
	private String proxyHost;
	@Value("${http.proxy.port}")
	private int proxyPort;

	private AmazonSQSClient client;

	@PostConstruct
	private void init() {
		credentials = new BasicAWSCredentials(accessKey, secretKey);
		config = new ClientConfiguration();

		config.setMaxConnections(maxConnections);
		config.setMaxErrorRetry(maxErrorRetry);
		config.withTcpKeepAlive(true);

		if (proxyEnabled) {
			config.setProxyHost(proxyHost);
			config.setProxyPort(proxyPort);
		}
		regionObject = Region.getRegion(Regions.fromName(region));
	}

	@Override
	public AmazonSQSClient createClient() {
		if (client == null) {
			client = new AmazonSQSClient(this.credentials, this.config);
			client.setRegion(this.regionObject);
		}
		return client;
	}

}
