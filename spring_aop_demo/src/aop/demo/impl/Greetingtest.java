package aop.demo.impl;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;



import aop.demo.Greeting;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("/demo-spring.xml")
@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)

public class Greetingtest {
	
	@Autowired
	Greeting greeting;
	
	@Test
	public void testGetUserInfo() throws Exception{
		greeting.sayHello("hardy");
	}
}
