package service;

public interface GatewayService {

	public void checkPassword(String token, String password, String blockId);
	
	public String encryptFile(String token, String password, String blockId);
	
	public String decryptFile(String token, String password, String blockId);
}
