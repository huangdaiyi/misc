package service.impl;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Enumeration;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.stereotype.Component;

import service.RPCService;
import utils.StringUtils;
import constants.HttpStatus;
import exception.MetadataException;

@Component
public class RPCServiceImpl implements RPCService {
	
	private static final Logger logger = LogManager.getLogger(RPCServiceImpl.class);
	
	public Object call(String serverName,HttpServletRequest request,Object body) throws MetadataException{
		Object object = null;

		HttpURLConnection connection = null;
		DataOutputStream requestStream = null;

		BufferedReader reader = null;
		int code = 500;

		try {
			String url = "http://" + serverName + ":" + request.getServerPort()
					+ request.getRequestURI();
			connection = (HttpURLConnection) new URL(url).openConnection();
			connection.setDoOutput(true);
			connection.setUseCaches(false);

			Enumeration<String> headers = request.getHeaderNames();

			while (headers.hasMoreElements()) {
				String headerName = headers.nextElement();
				if (!("host".equalsIgnoreCase(headerName) || "content-length"
						.equalsIgnoreCase(headerName))) {
					connection.setRequestProperty(headerName,
							request.getHeader(headerName));
				}
			}

			if (body != null) {
				String content = StringUtils.writeJSON(body);

				// Send request
				requestStream = new DataOutputStream(
						connection.getOutputStream());
				requestStream.writeBytes(content);
				requestStream.flush();
			}
			// Get Response
			code = connection.getResponseCode();
			if (code == 200) {
				object = connection.getContent();
			}
		} catch (Exception ex) {
			logger.error("IRPC error", ex);
		} finally {
			IOUtils.closeQuietly(requestStream);
			IOUtils.closeQuietly(reader);
			
			if (connection != null) {
				connection.disconnect();
			}
		}

		if (object == null) {
			throw new MetadataException(HttpStatus.fromCode(code));
		}

		return object;
	}
}
