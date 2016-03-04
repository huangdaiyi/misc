package aop.impl;

import static org.junit.Assert.assertEquals;

import java.net.InetAddress;
import java.net.UnknownHostException;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import model.DBLocation;
import model.RequestBase;
import model.UserBlock;

import org.aspectj.lang.ProceedingJoinPoint;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import partition.DBLocationService;
import service.RPCService;
import service.UserAuthorizationService;
import constants.HttpStatus;
import exception.MetadataException;
import factory.UserBlockFactory;

public class ControllerAspectTest {
	
	@Mock 
	private HttpServletRequest request;
	
	@Mock 
	private HttpServletResponse response;
	
	@Mock
	private UserAuthorizationService checkAuth;
	
	@Mock
	private UserAuthorizationService ssoCheckAuth;
	
	@Mock
	private DBLocationService dbLocationService;
	
	@Mock
	private RPCService rpc;
	
	@Mock
	private UserBlockFactory userBlockMap;
	
	@InjectMocks
	private ControllerAspect controllerAspect;
	
	private String hostName;
	
	@Before
	public void initMocks(){
		MockitoAnnotations.initMocks(this);
		Mockito.when(userBlockMap.getBlockByUserId(Mockito.anyString())).thenReturn(new UserBlock());
		try {
			hostName = InetAddress.getLocalHost().getHostName();
		} catch (UnknownHostException e) {
			e.printStackTrace();
		}
	}

	@Test
	public void controllerBefor_Header_UNAUTHORIZED_Test(){
		HttpStatus status = HttpStatus.OK;
		ProceedingJoinPoint joinPoint = Mockito.mock(ProceedingJoinPoint.class);
		Object[] objs = new Object[1];
		RequestBase arg = new RequestBase();
		objs[0] = arg;
		Mockito.when(joinPoint.getArgs()).thenReturn(objs);
		try{
			controllerAspect.controllerBefor(joinPoint);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.TOKEN_IS_INVALID);
		
		status = HttpStatus.OK;
		Mockito.when(request.getHeader("neweggbox-sso-token")).thenReturn("roger");
		
		Mockito.when( checkAuth.checkToken("roger")).thenReturn(null);
		
		try{
			controllerAspect.controllerBefor(joinPoint);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.TOKEN_IS_INVALID);
	}
	
	@Test
	public void controllerBefor_Header_Test(){
		
		ProceedingJoinPoint joinPoint = Mockito.mock(ProceedingJoinPoint.class);
		Object[] objs = new Object[1];
		RequestBase arg = new RequestBase();
		objs[0] = arg;
		Mockito.when(joinPoint.getArgs()).thenReturn(objs);
		Mockito.when(request.getHeader("neweggbox-sso-token")).thenReturn("roger");
		RequestBase requestBase = new RequestBase();
		requestBase.setDeviceId("test_device");
		requestBase.setUserId("roger_id");
		
		Mockito.when( checkAuth.checkToken("roger")).thenReturn(requestBase);
		
		DBLocation dbLoaction = new DBLocation();
		dbLoaction.setHost("10.16.76.227");
		dbLoaction.setPort(456);
		dbLoaction.setDbInstance("NeweggBox");
		dbLoaction.setServerName(hostName);
		
		Mockito.when(dbLocationService.locateDB("roger_id")).thenReturn(dbLoaction);
		
		controllerAspect.controllerBefor(joinPoint);
		
		assertEquals(arg.getToken(), "roger");
		assertEquals(arg.getDbHost(), "10.16.76.227");
		assertEquals(arg.getDbInstance() ,"NeweggBox");
		assertEquals(arg.getDbPort() ,456);
		assertEquals(arg.getUserId() ,"roger_id");
		assertEquals(arg.getDeviceId() ,"test_device");
	}
	
	
	@Test
	public void controllerBefor_Cookie_UNAUTHORIZED_Test(){
		HttpStatus status = HttpStatus.OK;
		ProceedingJoinPoint joinPoint = Mockito.mock(ProceedingJoinPoint.class);
		Object[] objs = new Object[1];
		objs[0] = new RequestBase();
		Mockito.when(joinPoint.getArgs()).thenReturn(objs);
		
		try{
			controllerAspect.controllerBefor(joinPoint);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.TOKEN_IS_INVALID);
		
		status = HttpStatus.OK;
		Cookie[] cookies = new Cookie[1];
		cookies[0] = new Cookie("neweggbox-sso-token","roger's cookie");
		
		Mockito.when(request.getCookies()).thenReturn(cookies);
		
		Mockito.when(checkAuth.checkToken("roger's cookie")).thenReturn(null);
		
		try{
			controllerAspect.controllerBefor(joinPoint);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.TOKEN_IS_INVALID);
	}
	
	@Test
	public void controllerBefor_Cookie_Test() throws NoSuchMethodException, SecurityException{
		
		ProceedingJoinPoint joinPoint = Mockito.mock(ProceedingJoinPoint.class);
		Object[] objs = new Object[1];
		RequestBase arg = new RequestBase();
		objs[0] = arg;
		Mockito.when(joinPoint.getArgs()).thenReturn(objs);
		
		Cookie[] cookies = new Cookie[1];
		cookies[0] = new Cookie("neweggbox-sso-token","roger's cookie");
		Mockito.when(request.getCookies()).thenReturn(cookies);
		
		RequestBase requestBase = new RequestBase();
		requestBase.setDeviceId("test_device");
		requestBase.setUserId("roger_id");
		
		Mockito.when( checkAuth.checkToken("roger's cookie")).thenReturn(requestBase);
		
		DBLocation dbLoaction = new DBLocation();
		dbLoaction.setHost("10.16.76.227");
		dbLoaction.setPort(456);
		dbLoaction.setDbInstance("NeweggBox");
		dbLoaction.setServerName(hostName);
		
		Mockito.when(dbLocationService.locateDB("roger_id")).thenReturn(dbLoaction);
		
		
		controllerAspect.controllerBefor(joinPoint);
		
		assertEquals(arg.getToken(), "roger's cookie");
		assertEquals(arg.getDbHost(), "10.16.76.227");
		assertEquals(arg.getDbInstance() ,"NeweggBox");
		assertEquals(arg.getDbPort() ,456);
		assertEquals(arg.getUserId() ,"roger_id");
		assertEquals(arg.getDeviceId() ,"test_device");
	}
	
	
	@Test
	public void controllerBefor_Locate_Null_Test(){
		
		ProceedingJoinPoint joinPoint = Mockito.mock(ProceedingJoinPoint.class);
		Object[] objs = new Object[1];
		RequestBase arg = new RequestBase();
		objs[0] = arg;
		Mockito.when(joinPoint.getArgs()).thenReturn(objs);
		
		Cookie[] cookies = new Cookie[1];
		cookies[0] = new Cookie("neweggbox-sso-token","roger's cookie");
		Mockito.when(request.getCookies()).thenReturn(cookies);
		
		RequestBase requestBase = new RequestBase();
		requestBase.setDeviceId("test_device");
		requestBase.setUserId("roger_id");
		
		Mockito.when( checkAuth.checkToken("roger's cookie")).thenReturn(requestBase);
		
		DBLocation dbLoaction = new DBLocation();
		dbLoaction.setHost("10.16.76.227");
		dbLoaction.setPort(456);
		dbLoaction.setDbInstance("NeweggBox");
		dbLoaction.setServerName(hostName);
		
		Mockito.when(dbLocationService.residentDB("roger_id",hostName)).thenReturn(dbLoaction);
		
		
		controllerAspect.controllerBefor(joinPoint);
		
		assertEquals(arg.getToken(), "roger's cookie");
		assertEquals(arg.getDbHost(), "10.16.76.227");
		assertEquals(arg.getDbInstance() ,"NeweggBox");
		assertEquals(arg.getDbPort() ,456);
		assertEquals(arg.getUserId() ,"roger_id");
		assertEquals(arg.getDeviceId() ,"test_device");
	}
	
	@Test
	public void controllerBefor_Locate_Null_Exception_Test(){
		
		ProceedingJoinPoint joinPoint = Mockito.mock(ProceedingJoinPoint.class);
		Object[] objs = new Object[1];
		RequestBase arg = new RequestBase();
		objs[0] = arg;
		Mockito.when(joinPoint.getArgs()).thenReturn(objs);
		
		Cookie[] cookies = new Cookie[1];
		cookies[0] = new Cookie("neweggbox-sso-token","roger's cookie");
		Mockito.when(request.getCookies()).thenReturn(cookies);
		
		RequestBase requestBase = new RequestBase();
		requestBase.setDeviceId("test_device");
		requestBase.setUserId("roger_id");
		
		Mockito.when( checkAuth.checkToken("roger's cookie")).thenReturn(requestBase);
		
		Mockito.when(dbLocationService.residentDB("roger_id",hostName)).thenReturn(null);
		MetadataException metadataException = null;
		try	{
			controllerAspect.controllerBefor(joinPoint);
		}catch(MetadataException ex){
			metadataException = ex;
		}
		
		assertEquals(metadataException.getHttpStatus(), HttpStatus.INTERNAL_SERVER_ERROR);
	}

	@Test
	public void controllerBefor_Remote_Test(){
		
		ReflectionTestUtils.setField(controllerAspect, "cluster", true);
		
		ProceedingJoinPoint joinPoint = Mockito.mock(ProceedingJoinPoint.class);
		Object[] objs = new Object[1];
		RequestBase arg = new RequestBase();
		objs[0] = arg;
		Mockito.when(joinPoint.getArgs()).thenReturn(objs);
		
		Cookie[] cookies = new Cookie[1];
		cookies[0] = new Cookie("neweggbox-sso-token","roger's cookie");
		Mockito.when(request.getCookies()).thenReturn(cookies);
		
		RequestBase requestBase = new RequestBase();
		requestBase.setDeviceId("test_device");
		requestBase.setUserId("roger_id");
		
		Mockito.when( checkAuth.checkToken("roger's cookie")).thenReturn(requestBase);
		
		DBLocation dbLoaction = new DBLocation();
		dbLoaction.setHost("10.16.76.227");
		dbLoaction.setPort(456);
		dbLoaction.setDbInstance("NeweggBox");
		dbLoaction.setServerName("testserver");
		
		Mockito.when(dbLocationService.locateDB("roger_id")).thenReturn(dbLoaction);
		String response = "Hello World";
		Mockito.when(rpc.call(Mockito.anyString(), Mockito.any(HttpServletRequest.class), Mockito.anyObject())).thenReturn(response);
		
		assertEquals("Hello World",controllerAspect.controllerBefor(joinPoint));
		
		assertEquals(arg.getToken(), "roger's cookie");
		assertEquals(arg.getDbHost(), "10.16.76.227");
		assertEquals(arg.getDbInstance() ,"NeweggBox");
		assertEquals(arg.getDbPort() ,456);
		assertEquals(arg.getUserId() ,"roger_id");
		assertEquals(arg.getDeviceId() ,"test_device");
	}
	
	@Test
	public void controllerBefor_RPC_Test() throws Throwable{
		
		ReflectionTestUtils.setField(controllerAspect, "cluster", true);
		
		ProceedingJoinPoint joinPoint = Mockito.mock(ProceedingJoinPoint.class);
		Object[] objs = new Object[1];
		RequestBase arg = new RequestBase();
		arg.setDeviceId("test_device");
		arg.setUserId("roger_id");
		arg.setDbPort(456);
		arg.setDbHost("10.16.76.227");
		arg.setDbInstance("NeweggBox");
		arg.setToken("test token");
		objs[0] = arg;
		
		Mockito.when(joinPoint.getArgs()).thenReturn(objs);
		Mockito.when(joinPoint.proceed()).thenReturn("Hello world");
		
		
		assertEquals("Hello world",controllerAspect.controllerBefor(joinPoint));
			
		assertEquals(arg.getToken(), "test token");
		assertEquals(arg.getDbHost(), "10.16.76.227");
		assertEquals(arg.getDbInstance() ,"NeweggBox");
		assertEquals(arg.getDbPort() ,456);
		assertEquals(arg.getUserId() ,"roger_id");
		assertEquals(arg.getDeviceId() ,"test_device");
	}
	
}
