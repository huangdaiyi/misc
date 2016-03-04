package factory;

import model.UserBlock;

public interface UserBlockFactory {

	public UserBlock getBlockByUserId(String userId);

}
