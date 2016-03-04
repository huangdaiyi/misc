package aop.demo;

public interface AopAction {

	public Object[] before(Object[] args);

	public Object[] after(Object[] args);

	public void afterReturning(Object result);

	public void afterThrowing(Throwable e);

}