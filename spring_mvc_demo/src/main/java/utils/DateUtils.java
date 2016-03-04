package utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import constants.HttpStatus;
import exception.MetadataException;

public final class DateUtils {

	private static TimeZone utcTimeZone = TimeZone.getTimeZone("UTC");
	private static String defaultDateFormator = "yyyy-MM-dd HH:mm:ss";

	public static String nowUTCDateTime() {
		SimpleDateFormat sdf = new SimpleDateFormat(defaultDateFormator);
		sdf.setTimeZone(utcTimeZone);
		Date date = new Date();
		return sdf.format(date);
	}

	public static long nowUTCTimestamp() {
		// millis seconed is utc base, like System.currentTimeMillis()
		return Calendar.getInstance(utcTimeZone).getTimeInMillis();
	}

	public static Long convertDatetime(String strTime, String formator) {
		SimpleDateFormat dateFormat = new SimpleDateFormat(formator);
		dateFormat.setTimeZone(utcTimeZone);
		Long timestamp = 0L;
		try {
			timestamp = dateFormat.parse(strTime).getTime();
		} catch (ParseException e) {
		}
		return timestamp;
	}

	public static String trimToDate(String strDateTime, String formator) {
		SimpleDateFormat dateFormat = new SimpleDateFormat(formator);
		Date date = null;
		String dateStr = "";
		try {
			date = dateFormat.parse(strDateTime);
			dateStr = dateFormat.format(date);
		} catch (ParseException e) {
		}
		return dateStr;
	}

	public static String convertDatetime(long timestamp, String formator) {
		SimpleDateFormat sdf = new SimpleDateFormat(formator);
		sdf.setTimeZone(utcTimeZone);
		Date date = new Date(timestamp);
		return sdf.format(date);
	}

	public static Long convertDatetime(String strTime) {
		if (StringUtils.isNullOrEmpty(strTime)) {
			return null;
		}
		String tempString = strTime.trim();
		int len = tempString.length();
		if (len == 10) { // yyyy-MM-dd
			tempString = tempString + " 00:00:00";
		} else if (len != 19) { // yyyy-MM-dd HH:mm:ss
			throw new MetadataException(HttpStatus.ERROR_IN_PARAMETERS);
		}
		return convertDatetime(tempString, defaultDateFormator);
	}

	public static String convertDatetime(long timestamp) {
		return convertDatetime(timestamp, defaultDateFormator);
	}

}
