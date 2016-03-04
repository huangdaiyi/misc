package service.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.HttpHost;
import org.apache.http.NameValuePair;
import org.apache.http.conn.params.ConnRoutePNames;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import service.SMSService;

import com.twilio.sdk.TwilioRestClient;
import com.twilio.sdk.TwilioRestException;
import com.twilio.sdk.resource.factory.MessageFactory;

@Service
public class SMSServiceImpl implements SMSService {
	private static final Logger logger = LogManager.getLogger(SMSServiceImpl.class);

	@Value("${twilio.account}")
	private String twilioAccount;
	@Value("${twilio.token}")
	private String twilioToken;
	@Value("${http.proxy.enabled}")
	private boolean proxyEnabled;
	@Value("${http.proxy.host}")
	protected String proxyHost;
	@Value("${http.proxy.port}")
	protected int proxyPort;
	@Value("${sso.host}")
	private String host;

	@SuppressWarnings("deprecation")
	@Override
	public boolean sendByTwilio(String message, String recipient) {
		boolean isSendSMSSuccess = false;
		TwilioRestClient client = new TwilioRestClient(twilioAccount,twilioToken);
		if (proxyEnabled) {
			HttpHost proxy = new HttpHost(proxyHost, proxyPort);
//			DefaultProxyRoutePlanner routePlanner1 = new DefaultProxyRoutePlanner(
//					proxy);
//			CloseableHttpClient httpclient = HttpClients.custom()
//					.setRoutePlanner(routePlanner1).build();
			DefaultHttpClient httpclient = new DefaultHttpClient();
			httpclient.getParams().setParameter(ConnRoutePNames.DEFAULT_PROXY, proxy);
			client.setHttpClient(httpclient);

		}

		List<NameValuePair> params = new ArrayList<NameValuePair>();
		params.add(new BasicNameValuePair("Body",message));
		params.add(new BasicNameValuePair("To", "+"+recipient));
		params.add(new BasicNameValuePair("From", "+16266049142"));
		MessageFactory  messageFactory = client.getAccount().getMessageFactory();
		try {
			messageFactory.create(params);
			
			isSendSMSSuccess = true;
		} catch (TwilioRestException e) {
			logger.error("SendSMSError", e);
		}

		return isSendSMSSuccess;
	}
	
}
