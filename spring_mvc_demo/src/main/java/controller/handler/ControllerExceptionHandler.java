package controller.handler;

import java.io.IOException;

import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import utils.StringUtils;
import constants.HttpStatus;
import exception.MetadataException;

@ControllerAdvice
public class ControllerExceptionHandler {
	private static final Logger logger = LogManager.getLogger(ControllerExceptionHandler.class);

	@ExceptionHandler(MetadataException.class)
	public void handleException(MetadataException ex, HttpServletResponse response) throws IOException {
		HttpStatus httpStatus = ex.getHttpStatus();
		Object responseData = ex.getResponseData();

		if (ex.getHttpStatus() == HttpStatus.INTERNAL_SERVER_ERROR) {
			logger.error("handleException: " + httpStatus.name(), ex);
		} else {
			logger.warn("handleException: " + httpStatus.name());
			logger.debug("handleException: " + httpStatus.name(), ex);
		}
		if (responseData == null) {
			response.sendError(httpStatus.getCode(), httpStatus.getReason());
		} else {
			response.setStatus(httpStatus.getCode());
			response.getWriter().write(StringUtils.writeJSON(responseData));
		}
	}

	@ExceptionHandler(HttpMediaTypeNotSupportedException.class)
	public void handleException(HttpMediaTypeNotSupportedException ex, HttpServletResponse response) throws IOException {
		HttpStatus httpStatus = HttpStatus.METHOD_NOT_ALLOWED;
		response.sendError(httpStatus.getCode(), ex.getMessage());
	}

	public void handleException(Exception ex, HttpServletResponse response) throws IOException {
		HttpStatus httpStatus = HttpStatus.INTERNAL_SERVER_ERROR;
		logger.error("handleException", ex);
		response.sendError(httpStatus.getCode(), httpStatus.getReason());
	}

}
