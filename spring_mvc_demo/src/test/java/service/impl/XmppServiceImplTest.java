package service.impl;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import service.XmppService;
import base.BaseTestCase;

public class XmppServiceImplTest extends BaseTestCase {

	@Autowired
	private XmppService xmppService;

	@Test
	public void testXmpp() {
		String userId = "73450575e5c049baf03e53d3540edd64";
		String deviceId = "all";
		xmppService.sendMessage(userId, deviceId, "hello world!");
		try {
			Thread.sleep(500);
		} catch (InterruptedException e) {
		}
	}

}
