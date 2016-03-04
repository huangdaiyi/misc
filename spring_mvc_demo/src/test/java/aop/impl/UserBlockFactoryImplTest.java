package aop.impl;

import static org.junit.Assert.assertEquals;

import java.util.Calendar;

import model.UserBlock;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import factory.impl.UserBlockFactoryImpl;

public class UserBlockFactoryImplTest {
	
	@InjectMocks
	private UserBlockFactoryImpl impl;
	
	@Before
	public void initMocks(){
	
		MockitoAnnotations.initMocks(this);	
		ReflectionTestUtils.setField(impl, "timeout", 1000);
		impl.init();
	}
	
	@Test
	public void getBlockByUserIdTest() throws InterruptedException{
		UserBlock userBlock = impl.getBlockByUserId("roger");
		Object block = userBlock.getBlock();
		Calendar time = userBlock.getDate();
		
		assertEquals(true,userBlock.getDate() != null);
		assertEquals(true,block != null);
		
		userBlock = impl.getBlockByUserId("roger");
		
		assertEquals(true,userBlock.getDate() != time);
		assertEquals(true,userBlock.getBlock() == block);
		
		
		Thread.sleep(2000);
		
		UserBlock newUserBlock = impl.getBlockByUserId("roger");
		Object newBlock = newUserBlock.getBlock();
		
		assertEquals(true,newBlock != null);
		assertEquals(true,time != newUserBlock.getDate());
		assertEquals(true,newUserBlock.getDate() != null);
		assertEquals(true,newUserBlock != userBlock);
		assertEquals(true,block != newBlock);
	}
}
