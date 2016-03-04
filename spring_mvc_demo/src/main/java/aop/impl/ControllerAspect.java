package aop.impl;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.UnknownHostException;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

import model.BulkRequestBase;
import model.DBLocation;
import model.IRequestBase;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.Signature;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import partition.DBLocationService;
import service.DynamoDBService;
import service.RPCService;
import service.SSOService;
import utils.StringUtils;
import annotation.AdminAuthorizationOnly;
import annotation.AllowAdminAuthorization;
import annotation.Readonly;
import constants.HttpStatus;
import exception.MetadataException;
import factory.UserBlockFactory;

@Aspect
@Component
public class ControllerAspect {
	private static final Logger logger = LogManager.getLogger(ControllerAspect.class);

	private static String hostName = null;

	static {
		try {
			hostName = InetAddress.getLocalHost().getHostName();
		} catch (UnknownHostException e) {
			logger.error("get host name error", e);
		}
	}

	@Value("${neweggbox.cluster}")
	private boolean cluster;
	@Value("${dblocation.enabled}")
	private boolean dblocationEnabled;
	@Value("${adminToken}")
	private String adminToken;

	@Autowired
	private HttpServletRequest request;

	@Autowired
	private DynamoDBService dynamoDBService;
	@Autowired
	private SSOService ssoService;
	@Autowired
	private RPCService rpc;
	@Autowired
	private UserBlockFactory userBlockMap;
	@Autowired
	private DBLocationService dbLocationService;

	@Around("execution(public * controller.impl.*.*(..))")
	public Object controllerBefor(ProceedingJoinPoint proceedingJoinPoint) throws MetadataException {
		Object responseObject = null;
		for (Object param : proceedingJoinPoint.getArgs()) {
			if (param instanceof IRequestBase) {
				IRequestBase requestBaseParam = (IRequestBase) param;
				if (isRPC(requestBaseParam)) {
					responseObject = localExecute(requestBaseParam.getUserId(), proceedingJoinPoint, false);
				} else {
					responseObject = requestExecute(requestBaseParam, proceedingJoinPoint);
				}
				break;
			}
		}
		return responseObject;
	}

	private boolean isRPC(IRequestBase requestBaseParam) {
		return cluster && StringUtils.isNullOrEmpty(requestBaseParam.getDbHost()) == false && StringUtils.isNullOrEmpty(requestBaseParam.getDbInstance()) == false
				&& StringUtils.isNullOrEmpty(requestBaseParam.getUserId()) == false && StringUtils.isNullOrEmpty(requestBaseParam.getToken()) == false;
	}

	private String fetchSSOToken() {
		String token = request.getHeader("neweggbox-sso-token");
		// token
		if (token == null && request.getCookies() != null) {
			for (Cookie cookie : request.getCookies()) {
				if ("neweggbox-sso-token".equalsIgnoreCase(cookie.getName())) {
					token = cookie.getValue();
					break;
				}
			}
		}
		return token;
	}

	private String fetchAuthorization() {
		return request.getHeader("Authorization");
	}

	private IRequestBase checkSSOToken(String token) {
		IRequestBase requestBase = null;
		if (token != null) {
			requestBase = dynamoDBService.checkToken(token);
			if (requestBase == null) {
				requestBase = ssoService.checkToken(token);
			}
		}
		return requestBase;
	}

	private DBLocation locateDatabase(String userId) {
		if (userId == null || userId.isEmpty()) {
			logger.error("locate db error, userId could be empty.");
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
		DBLocation dbLocation = dbLocationService.locateDB(userId);
		if (dbLocation == null) {
			dbLocation = dbLocationService.residentDB(userId, hostName);
		}
		if (dbLocation == null) {
			logger.error("locate db error, availabledb could be empty.");
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
		return dbLocation;
	}

	private void checkAndUpdateUserInfo(IRequestBase requestBaseParam, ProceedingJoinPoint proceedingJoinPoint) {
		boolean adminAuthorizationOnly = includeMethodSignature(proceedingJoinPoint, AdminAuthorizationOnly.class);
		// neweggbox-sso-token
		String token = fetchSSOToken();
		if (token != null && adminAuthorizationOnly == false) {
			IRequestBase requestBase = checkSSOToken(token);
			if (requestBase != null && StringUtils.isNullOrEmpty(requestBase.getUserId()) == false) {
				requestBaseParam.setToken(token);
				requestBaseParam.setDeviceId(requestBase.getDeviceId());
				requestBaseParam.setUserId(requestBase.getUserId());
				return;
			}
		}
		// authorization
		if (adminAuthorizationOnly || includeMethodSignature(proceedingJoinPoint, AllowAdminAuthorization.class)) {
			String authorization = StringUtils.decodeBase64(fetchAuthorization());
			String[] authorizationArray = authorization.split(":");
			String userId = authorizationArray[0];
			token = authorizationArray[1];
			if (adminToken.equals(token) && StringUtils.isNullOrEmpty(userId) == false) {
				requestBaseParam.setUserId(userId);
				requestBaseParam.setToken(token);
				return;
			}
		}
		throw new MetadataException(HttpStatus.TOKEN_IS_INVALID);
	}

	private void checkAndUpdateDBLocation(IRequestBase requestBaseParam) {
		DBLocation dbLocation = locateDatabase(requestBaseParam.getUserId());
		requestBaseParam.setDbHost(dbLocation.getHost());
		requestBaseParam.setDbPort(dbLocation.getPort());
		requestBaseParam.setDbInstance(dbLocation.getDbInstance());
		requestBaseParam.setDbServerName(dbLocation.getServerName());
	}

	private Object requestExecute(IRequestBase requestBaseParam, ProceedingJoinPoint proceedingJoinPoint) {
		// user info
		checkAndUpdateUserInfo(requestBaseParam, proceedingJoinPoint);
		// db location
		if (dblocationEnabled) {
			checkAndUpdateDBLocation(requestBaseParam);
		}
		// update params for bulk action
		if (requestBaseParam instanceof BulkRequestBase) {
			((BulkRequestBase<?>) requestBaseParam).eachFromRequestBase();
		}
		Object responseObject = null;
		if (dblocationEnabled) {
			boolean isReadonly = includeMethodSignature(proceedingJoinPoint, Readonly.class);
			if (isReadonly == false && isRemote(requestBaseParam.getDbServerName())) {
				responseObject = rpc.call(requestBaseParam.getDbServerName(), request, requestBaseParam);
			} else {
				responseObject = localExecute(requestBaseParam.getUserId(), proceedingJoinPoint, isReadonly);
			}
		} else {
			responseObject = localExecute(requestBaseParam.getUserId(), proceedingJoinPoint, false);
		}
		return responseObject;
	}

	private boolean isRemote(String serverName) {
		return this.cluster && hostName.equalsIgnoreCase(serverName) == false;
	}

	private Object localExecute(String userId, ProceedingJoinPoint proceedingJoinPoint, boolean isReadyonly) throws MetadataException {
		if (isReadyonly) {
			return execute(proceedingJoinPoint);
		}
		synchronized (userBlockMap.getBlockByUserId(userId)) {
			return execute(proceedingJoinPoint);
		}
	}

	private <T extends Annotation> boolean includeMethodSignature(ProceedingJoinPoint proceedingJoinPoint, Class<T> type) {
		boolean result = false;
		Signature joinPointSignature = proceedingJoinPoint.getSignature();
		if (joinPointSignature instanceof MethodSignature) {
			MethodSignature signature = (MethodSignature) joinPointSignature;
			Method method = signature.getMethod();
			result = (method.getAnnotation(type) != null);
		}
		return result;
	}

	private Object execute(ProceedingJoinPoint proceedingJoinPoint) throws MetadataException {
		try {
			return proceedingJoinPoint.proceed();
		} catch (MetadataException me) {
			throw me;
		} catch (Throwable e) {
			logger.error("execute error", e);
			throw new MetadataException(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

}
