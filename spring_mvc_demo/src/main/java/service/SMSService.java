package service;


public interface SMSService {
	public boolean sendByTwilio(String message,String recipient);
	
}
