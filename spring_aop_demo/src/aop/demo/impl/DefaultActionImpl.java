package aop.demo.impl;

import org.springframework.stereotype.Component;

import aop.demo.AopAction;

@Component
public class DefaultActionImpl implements AopAction {
	
	/* (non-Javadoc)
	 * @see aop.demo.impl.AopAction#before()
	 */
	@Override
	public Object[] before(Object[] args) {
		String name = (String)args[0];
		name = "before" + name;
		args[0] = name;
		System.out.println("before");
		return args;
	}

	/* (non-Javadoc)
	 * @see aop.demo.impl.AopAction#after()
	 */
	@Override
	public Object[] after(Object[] args) {
		System.out.println("after");
		return args;
	}

	/* (non-Javadoc)
	 * @see aop.demo.impl.AopAction#afterReturning(java.lang.Object)
	 */
	@Override
	public void afterReturning(Object result) {
		System.out.println("afterReturning");
	}

	/* (non-Javadoc)
	 * @see aop.demo.impl.AopAction#afterThrowing(java.lang.Throwable)
	 */
	@Override
	public void afterThrowing(Throwable e) {
		System.out.println("afterThrowing");
	}

}
