package aop.demo.impl;

import java.lang.reflect.Method;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.stereotype.Component;

import aop.demo.AopAction;
import aop.demo.Greeting;

@Aspect
@Component
@EnableAspectJAutoProxy(proxyTargetClass = true)
public class GreetingAspect {

	@Autowired
	private ApplicationContext appContext;

	private AopAction aopAction;

	private void initAopAction(String name) {
		/*
		 * MethodSignature signature = (MethodSignature) pjp.getSignature();
		 * Method method = signature.getMethod(); Tag tag =
		 * method.getAnnotation(Tag.class);
		 */
		aopAction = (AopAction) appContext.getBean(name);
		// aopAction = (AopAction) new
		// ClassPathXmlApplicationContext("demo-spring.xml").getBean(name);
	}

	@Around("execution(public * *(..)) && !@annotation(aop.demo.impl.NonTag) && (@annotation(aop.demo.impl.Tag) || @within(aop.demo.impl.Tag))")
	public Object around(ProceedingJoinPoint pjp) throws Throwable {
		initAopAction(getAopActionName(pjp));
		Object[] args = pjp.getArgs();
		aopAction.before(args);
		Object result = null;
		try {
			pjp.proceed(args);
		} catch (Exception e) {
			aopAction.afterThrowing(e);
		} finally {
			aopAction.afterReturning(result);
		}
		aopAction.after(args);
		return result;
	}

	private String getAopActionName(ProceedingJoinPoint pjp) {
		MethodSignature signature = (MethodSignature) pjp.getSignature();
		Method method = signature.getMethod();
		Tag tag = method.getAnnotation(Tag.class);
//		Assert.assertNotNull(tag);
//		org.springframework.util.Assert.notNull(tag);
		if(tag == null){
			tag = pjp.getTarget().getClass().getAnnotation(Tag.class);
		}
		org.apache.logging.log4j.core.util.Assert.requireNonNull(tag, "tag can not be null");
		return tag.actionName();
	}

}
