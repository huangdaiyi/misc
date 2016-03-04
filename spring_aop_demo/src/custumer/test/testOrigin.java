package custumer.test;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;

import com.google.common.base.Utf8;


public class testOrigin {
	
	
	public static void main(String[] args) {
		String test= ".";
		int index =  test.indexOf(".");
		String a = test.substring(0, index);
		A aa = new A();
		aa.setA("1");
		
		System.out.println(aa.getA());
		
		A aa1 = (A)aa.clone();
		aa1.setA("2");
		
		System.out.println(aa.getA());
		System.out.println(aa1.getA());
		
		String gbk = "iteye问答频道编码转换问题";  
        
        try {
			String iso = new String(gbk.getBytes(),"UTF-8");
			
			System.out.println(iso);
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}
	
	
}
