package service;

import model.RequestBase;

public interface UserAuthorizationService {

	public RequestBase checkToken(String token);

}
