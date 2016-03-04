package service.impl;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import service.SMSService;
import base.BaseTestCase;

public class SMSServiceImplTest extends BaseTestCase{
	@Autowired
	private SMSService smsService;
	
	@Value("${collaborate.inviteSMSTemplate}")
	private String collaborateInviteSMSTemplate;
	
	@Test
	public void testSMS(){
		String message = String.format(collaborateInviteSMSTemplate, "randy");
		smsService.sendByTwilio(message, "8860929226973");
		
	}

}
