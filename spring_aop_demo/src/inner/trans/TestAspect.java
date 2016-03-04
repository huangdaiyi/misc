package inner.trans;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class TestAspect {

    @Around("execution(* com.spring.aop.AopService.*(..)) || execution(* com.spring.aop.AopService..*.*(..))")
    public Object advice(ProceedingJoinPoint joinPoint) throws Throwable {
        System.out.println("aaa: TestAspect.advice(): " + joinPoint);
        return joinPoint.proceed();
    }

}