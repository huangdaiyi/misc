/**
 * 
 */
package aop.demo.impl;

import java.lang.reflect.Method;

import org.springframework.aop.framework.ProxyFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;






import aop.demo.Greeting;

/**
 * @author hh49
 *
 */
public class Client {

	public static void main(String[] args) throws Exception {
		 ProxyFactory proxyFactory = new ProxyFactory();
		 proxyFactory.setTarget(new GreetingImpl());
//		 proxyFactory.addAdvice(new GreetingBeforeAdvice());
//		 proxyFactory.addAdvice(new GreetingAfterAdvice());
		//
		// Greeting greeting = (GreetingImpl)proxyFactory.getProxy();
		ApplicationContext context = new ClassPathXmlApplicationContext(
				"demo-spring.xml"); // ªÒ»° Spring Context
		//Greeting greeting = (Greeting) context.getBean("greetingProxy"); 
		Greeting greeting = (Greeting) context.getBean("greetingImpl");
		
		//GreetingImpl greeting = new GreetingImpl();
	/*	try {
			Method m = greeting.getClass().getMethod("sayHello", String.class);
			Tag tag = m.getAnnotation(Tag.class);
			System.out.println( tag.actionName());
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}*/
//		Greeting g = (Greeting)context;
		
		//Greeting greeting = new GreetingImpl();
		greeting.sayHello("hardy1!!");
		
		greeting.sayHello2("hardy2!!");
	}

}
