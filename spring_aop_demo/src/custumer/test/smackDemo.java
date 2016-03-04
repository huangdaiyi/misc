package custumer.test;

import java.io.IOException;

import javax.security.sasl.SaslException;

import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.ChatManager;
import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode;
import org.jivesoftware.smack.MessageListener;
import org.jivesoftware.smack.SmackException;
import org.jivesoftware.smack.SmackException.NotConnectedException;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.tcp.XMPPTCPConnection;

public class smackDemo {
	public static void main(String[] args) {

		String xmppHost = "jabber.org", xmppPwd = "123456", user = "hardytest";
		//int xmppPort = 5222;
		ConnectionConfiguration config = new ConnectionConfiguration(xmppHost);
		config.setReconnectionAllowed(true);
		config.setSendPresence(true);
		config.setSecurityMode(SecurityMode.disabled);

		XMPPConnection connection = new XMPPTCPConnection(config);

		try {
			connection.connect();
		} catch (SmackException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XMPPException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}catch (Throwable e) {
			e.printStackTrace();
			// TODO: handle exception
		}

		try {
			connection.login(user, xmppPwd);
		} catch (SaslException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XMPPException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SmackException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}catch (Throwable e) {
			e.printStackTrace();
			// TODO: handle exception
		}
		
		
		
		Chat chat = ChatManager.getInstanceFor(connection).createChat(
				user, new MessageListener() {

					@Override
					public void processMessage(Chat chat, Message message) {
						String messageBody = message.getBody();  
		                System.out.println("收到信息："+ messageBody);  
					}
				});

		try {
			chat.sendMessage("hardy!");
		} catch (NotConnectedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XMPPException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}catch (Throwable e) {
			e.printStackTrace();
			// TODO: handle exception
		}
		
		
		try {
			connection.disconnect();
		} catch (NotConnectedException e1) {
			// TODO Auto-generated catch block
			System.out.println("not NotConnectedException");
			e1.printStackTrace();
		}
		
	}
	
	
}
