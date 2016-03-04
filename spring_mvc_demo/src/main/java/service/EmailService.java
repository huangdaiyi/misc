package service;

import model.request.EncryptRequest;

public interface EmailService {
//	use org.springframework.mail.javamail.JavaMailSenderImpl
//	ref:http://www.codejava.net/frameworks/spring/sending-e-mail-with-spring-mvc
	public void sendEncryptionNotificationMail(EncryptRequest encryptRequest);
	
	public boolean sendEmail(String to,String subject,String message);

}
