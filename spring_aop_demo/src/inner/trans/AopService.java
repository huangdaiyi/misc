package inner.trans;

import org.springframework.transaction.annotation.Transactional;

public class AopService {

    @Transactional
    public class InnerService {
        public void innerMethod() {
            System.out.println("xxx: AopService$InnerClass.innerMethod()");
        }
    }

    public void outerMethod() {
        System.out.println("xxx: AopService.outerMethod()");
        transactionalMethod();
    }

    @Transactional
    private void transactionalMethod() {
        System.out.println("xxx: AopService.transactionalMethod()");
    }
}