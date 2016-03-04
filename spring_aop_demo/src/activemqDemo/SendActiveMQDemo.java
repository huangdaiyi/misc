package activemqDemo;

import java.util.ArrayList;
import java.util.List;

public class SendActiveMQDemo {
	public static void main(String[] args) {
		String testPath = "123/q/c/d";
		System.out.println(testPath.substring(0, testPath.lastIndexOf("/")));
		List<String>  list = new ArrayList<String>();
		list.add("12312");
		list.forEach(a -> System.out.println(a));
		
		 System.out.println("123123");
	}
}
