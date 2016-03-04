package aop.demo.impl;



import org.springframework.stereotype.Component;

import aop.demo.Greeting;

//@Tag(actionName = "defaultActionImpl")
@Component
public class GreetingImpl implements Greeting {
  
    @Override
    @Tag(actionName = "defaultActionImpl")
    public void sayHello(String name) throws Exception {
    	
        System.out.println("Hello! " + name);
        //throw new Exception("hahh");
    }
    
    @Override
    //@NonTag
    public void sayHello2(String name) throws Exception {
    	
        System.out.println("Hello 2! " + name);
        //throw new Exception("hahh");
    }
}
