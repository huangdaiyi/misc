package utils;

import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.UUID;
import java.util.zip.CRC32;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.fasterxml.jackson.databind.ObjectMapper;

public class StringUtils {

	private static final Logger logger = LogManager.getLogger(StringUtils.class);
	private static ObjectMapper jsonMapper = new ObjectMapper();

	public static boolean isNullOrEmpty(String str) {
		return str == null ? true : str.isEmpty();
	}

	public static boolean isNullOrEmpty(List<?> list) {
		return list == null ? true : list.isEmpty();
	}

	public static boolean startsWith(String str, String prefix) {
		return isNullOrEmpty(str) == false && isNullOrEmpty(prefix) == false
				&& str.toLowerCase().startsWith(prefix.toLowerCase());
	}

	public static String toUpper(String str) {
		return str == null ? "" : str.toUpperCase();
	}

	public static String getUUID() {
		return UUID.randomUUID().toString();
	}

	public static <T> T readJSON(String jsonString, Class<T> clazz) {
		if (jsonString != null) {
			try {
				return jsonMapper.readValue(jsonString, clazz);
			} catch (Exception e) {
				logger.warn("readJSON error", e);
			}
		}
		return null;
	}

	public static String writeJSON(Object object) {
		try {
			return jsonMapper.writeValueAsString(object);
		} catch (Exception e) {
			logger.warn("writeJSON error", e);
		}
		return null;
	}

	public static String formatName(String name, int backupNo,
			boolean isBackupName) {
		if (StringUtils.isNullOrEmpty(name)) {
			return null;
		}
		int index = name.lastIndexOf('.');
		String ext = "", nameWithOutExt = name;
		if (index > 0) {
			ext = name.substring(index);
			nameWithOutExt = name.substring(0, index);
		}

		if (isBackupName) {
			index = name.lastIndexOf('_');
			if (index != -1) {
				nameWithOutExt = nameWithOutExt.substring(0, index);

			}
		}

		return String.format("%s_%s%s", nameWithOutExt, backupNo, ext);
	}

	public static String getMD5(String str) {
		return DigestUtils.md5Hex(str);
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

	public static String concatFilePath(String parentPath, String fileName) {
		if (parentPath == null || parentPath.isEmpty()) {
			return fileName;
		} else if (fileName == null || fileName.isEmpty()) {
			return parentPath;
		}
		return new StringBuilder().append(parentPath).append("/")
				.append(fileName).toString();
	}

	public static String trimFilePathRoot(String path) {
		int slashIndex = path.indexOf("/");
		if (slashIndex < 0) {
			return "";
		}
		return path.substring(slashIndex + 1);
	}
	
	public static String escapeHTML(String str) {
		return StringEscapeUtils.escapeHtml(str);
	}
	
	public static String encodeBase64(String str) {
		String base64String = "";
		if (str != null) {
			try {
				base64String = Base64.encodeBase64String(str.getBytes("utf-8"));
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
			}
		}
		return base64String;
	}

	public static String decodeBase64(String base64String) {
		String result = "";
		if (base64String != null) {
			try {
				result = new String(Base64.decodeBase64(base64String), "utf-8");
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
			}
		}
		return result;
	}
	
	public static String escapeSqlLikeString(String keyword) {
		String replaceKeyword = StringEscapeUtils.escapeSql(keyword);
		if(replaceKeyword.contains("_")){
			replaceKeyword = replaceKeyword.replace("_", "\\_");
		} 
		if(replaceKeyword.contains("%")){
			replaceKeyword = replaceKeyword.replace("%", "\\%");
		}
		return replaceKeyword;
	}

	public static String encryptAES128(String secretKey, String ivParam, String value) {
		try {
			IvParameterSpec iv = new IvParameterSpec(ivParam.getBytes("UTF-8"));
			SecretKeySpec skeySpec = new SecretKeySpec(secretKey.getBytes("UTF-8"), "AES");
			Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
			cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv);
			byte[] encrypted = cipher.doFinal(value.getBytes());
			return Base64.encodeBase64String(encrypted);
		} catch (Exception e) {
			logger.warn("encryptAES128 error: " + e.getMessage());
		}
		return "";
	}

	public static String decryptAES128(String secretKey, String ivParam, String encrypted) {
		try {
			IvParameterSpec iv = new IvParameterSpec(ivParam.getBytes("UTF-8"));
			SecretKeySpec skeySpec = new SecretKeySpec(secretKey.getBytes("UTF-8"), "AES");
			Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
			cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv);
			return new String(cipher.doFinal(Base64.decodeBase64(encrypted)));
		} catch (Exception e) {
			logger.warn("decryptAES128 error: " + e.getMessage());
		}
		return "";
	}

}