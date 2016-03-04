package custumer.test;


import java.io.UnsupportedEncodingException;
import java.text.DateFormat;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.UUID;
import java.util.zip.CRC32;


public class DateStringToLongTest {
	public static void main(String[] args) throws Exception {
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

		// String inputString = "00:01:30.500";
		
		String timeString = "3015-2-25 00:00:00";
		
		ParsePosition parsePosition = new ParsePosition(0);
		
		//parsePosition.
		//DateFormat.getDateInstance().f
		Date date = sdf.parse(timeString);
		System.out.println("in milliseconds: " + date.getTime());
		System.out.println(sdf.format(date));
		
		

		String crc32 = getCrc32(UUID.randomUUID().toString());
		
		System.out.println(crc32);
		System.out.println(getCrc32("htpqwklvynl=pojqhnulnxu"));
		System.out.println(getCrc32("rcoadjtslsa=gcpshlifrsp"));
		System.out.println(getCrc32("hydybfnyrpt=vtcdkivpjyt"));
		System.out.println(getCrc32("cmudwvdvmnb=svurvjgsmog"));
		List<Integer> testList =  new ArrayList<Integer>();
		for (int i = 0; i < 10; i++) {
			testList.add(i);
		}
		StringBuilder sb = new StringBuilder();
		
		for (Integer integer : testList) {
			System.out.println(integer);
			sb.insert(0, integer);
		}
		System.out.println(sb.toString());
		
		System.out.println(EnumTest.Cat.name());
		System.out.println(EnumTest.valueOf("Hardy").name());
		//System.out.println(EnumTest.Cat.valueOf(enumType, name));
		
		
		
	}

	
	public static String convertTime(long time){
	    Date date = new Date(time);
	    Format format = new SimpleDateFormat("yyyy MM dd HH:mm:ss");
	    return format.format(date);
	}
	
	public static String getCrc32(String str) {

		CRC32 crc = new CRC32();
		try {
			crc.update(str.getBytes("UTF-8"));
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException("UTF-8 encoding is not supported");
		}
		return Long.toHexString(crc.getValue());
	}
}