package service.impl;

import java.util.HashMap;

import model.request.EmailRequestBody;
import model.request.EncryptRequest;
import model.response.EmailResponse;
import model.response.SSOUserInfoResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import service.EmailService;
import service.RestService;
import service.SSOService;
import utils.DateUtils;

@Service
public class EmailServiceImpl implements EmailService {

	@Value("${email.service}")
	private String emailHost;
	@Value("${neweggbox.portal}")
	private String webPortalDomain;
	@Value("${gateway.external.url}")
	private String gatewayExternalUrl;
	@Value("${encrypt.subject}")
	private String encryptNotificationMailSubjectTemplate;
	@Value("${encrypt.notificationMailTemplate}")
	private String encryptNotificationMailTemplate;
	
	@Autowired
	private RestService restService;
	@Autowired
	private SSOService ssoService;

	private EmailRequestBody emailRequest;
	
	public void sendEncryptionNotificationMail(EncryptRequest encryptRequest) {
		SSOUserInfoResponse userInfo = ssoService.getUserInfoByToken(encryptRequest);
		if (userInfo.getEmails().size() == 0) {
			return;
		}
		String userMails = userInfo.getEmails().get(0);
		String userDisplayName = userInfo.getDisplayName();
		String userName = userInfo.getName();
		String changedExpressionPath = encryptRequest.getPath().replace('/','>');
		String lastEditTime = DateUtils.convertDatetime(DateUtils.nowUTCTimestamp());
		String subject = encryptNotificationMailSubjectTemplate;
		String password = encryptRequest.getPassword().replaceAll("(.+).{3}$", "$1***");
		String body = String.format(encryptNotificationMailTemplate, gatewayExternalUrl, 
				userDisplayName, userName, changedExpressionPath, lastEditTime, password);
		sendEmail(userMails, subject, body);
	}
	

	@Override
	public boolean sendEmail(String to, String subject, String message) {
		// creates a simple e-mail object
		boolean isSendEmail = false;
		HashMap<String, String> smtpSetting = new HashMap<String, String>();
		smtpSetting.put("SubjectEncoding", "UTF8");
		smtpSetting.put("BodyEncoding", "UTF8");

		emailRequest = new EmailRequestBody();
		emailRequest.setFrom("info@newegg.com");
		emailRequest.setTo(to);
		emailRequest.setSubject(subject);
		emailRequest.setBody(message);
		emailRequest.setNeedLog("false");
		emailRequest.setPriority("Normal");
		emailRequest.setContentType("Html");
		emailRequest.setMailType("Smtp");
		emailRequest.setSmtpSetting(smtpSetting);

		EmailResponse response = restService.sendPostRequestForObj(
				emailHost, null, emailRequest, EmailResponse.class);
		if (response != null && response.isSendSuccess()) {
			isSendEmail = true;
		}
		return isSendEmail;
	}

}
