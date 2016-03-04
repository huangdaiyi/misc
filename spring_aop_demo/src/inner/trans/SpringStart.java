package inner.trans;

import org.springframework.context.support.ClassPathXmlApplicationContext;

public class SpringStart {
	public static void main(String[] args) throws Exception {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("spring-config.xml");

        System.out.println("");
        System.out.println("");
        System.out.println("-------------------> Transacted inner bean example <-------------------");
        AopService.InnerService inner = (AopService.InnerService) context.getBean("inner");
        inner.innerMethod();


        System.out.println("");
        System.out.println("");
        System.out.println("-------------------> Transacted self-invocation example <-------------------");
        AopService outer = (AopService) context.getBean("outer");
        outer.outerMethod();
    }
}