package service.impl;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import service.EmailService;
import base.BaseTestCase;

public class EmailServiceImplTest extends BaseTestCase{
	@Autowired
	private EmailService emailServcie;
	@Value("${collaborate.inviteMailTemplate}")
	private String collaborateInviteMailTemplate;
	
	@Test
	public void testSendEamil(){
		String userName = "userA";
		String webPortal = "http://172.22.8.38:8010";
		String displayCellphones = "0912345678";
		String inviteCellphones = "0987654321";
		String body = String.format(collaborateInviteMailTemplate, userName, displayCellphones, webPortal, inviteCellphones, webPortal, inviteCellphones);
		emailServcie.sendEmail("Mervin.M.Huang@newegg.com", "subject", body);
	}

}
