package factory.impl;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;

import model.UserBlock;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import factory.UserBlockFactory;

@Component
public class UserBlockFactoryImpl implements UserBlockFactory,Runnable  {
	
	private static final Logger logger = LogManager.getLogger(UserBlockFactoryImpl.class);
	
	private Map<String,UserBlock> userBlockMaps = new HashMap<String,UserBlock>();

	@Value("${neweggbox.blockTimeout}")
	private long timeout = 3600000;
	
	@PostConstruct
	public void init(){
		Thread thread = new Thread(this);
		thread.setDaemon(true);
		thread.start();
	}
	
	@Override
	public UserBlock getBlockByUserId(String userId) {
		UserBlock result = userBlockMaps.get(userId);	
		
		if(result == null){
			result = new UserBlock();
			userBlockMaps.put(userId, result);
		}else{
			result.setDate(Calendar.getInstance());
		}
		return result;
	}
	
	@Override
	public void run() {
		
		try {
			Thread.sleep(timeout);
			while (true) {
				
				long now = Calendar.getInstance().getTimeInMillis();
				
				List<String> removeKeys = new ArrayList<String>();
				
				for(String key : userBlockMaps.keySet()){
					UserBlock block = userBlockMaps.get(key);
					if(now - block.getDate().getTimeInMillis() >= timeout){ 
						removeKeys.add(key);
					}
				}
				
				for(String key : removeKeys){
					userBlockMaps.remove(key);
				}
				
				Thread.sleep(timeout);
			}
		} catch (Exception ex) {
			logger.error("IUserBlockMap error", ex);
		}
	}

}
