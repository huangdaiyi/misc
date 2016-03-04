package custumer.test;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class TestConfig {
	
	public TestConfig() {
		 InputStream inputStream = this.getClass().getClassLoader().getResourceAsStream("config.properties");  
		 Properties p = new Properties();
		 try {
			p.load(inputStream);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			
		}
		 System.out.println("ip:"+p.getProperty("ip")+",port:"+p.getProperty("port"));
	}
	
	public static void main(String[] args) {
		 new TestConfig();
	}
}
