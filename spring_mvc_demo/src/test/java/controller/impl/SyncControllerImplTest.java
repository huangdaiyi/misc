package controller.impl;

import static org.junit.Assert.assertEquals;
import model.request.GetActivityHistoryRequest;
import model.response.GetActivityHistoryResponse;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import service.SyncService;
import constants.HttpStatus;
import exception.MetadataException;

public class SyncControllerImplTest {
	@Mock
	private SyncService service;
	
	@InjectMocks
	private SyncControllerImpl impl;
	
	@Before
	public void initMocks(){
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void testSearchHistory(){
		GetActivityHistoryRequest getActivityHistoryRequest = Mockito.mock(GetActivityHistoryRequest.class);
		
		GetActivityHistoryResponse getActivityHistoryResponse = Mockito.mock(GetActivityHistoryResponse.class);
		Mockito.when(getActivityHistoryRequest.getPath()).thenReturn("mock path");
		
		Mockito.when(service.searchHistory(getActivityHistoryRequest)).thenReturn(getActivityHistoryResponse);
		
		GetActivityHistoryResponse response = impl.searchHistory(getActivityHistoryRequest);
		assertEquals(getActivityHistoryResponse, response);
	}
	
	@Test
	public void testSearchHistoryBadReqeust(){
		GetActivityHistoryRequest getActivityHistoryRequest = Mockito.mock(GetActivityHistoryRequest.class);

		Mockito.when(getActivityHistoryRequest.getPath()).thenReturn("");
		HttpStatus status = HttpStatus.OK;
		try{
			impl.searchHistory(getActivityHistoryRequest);
		}catch(MetadataException ex){
			status = ex.getHttpStatus();
		}
		assertEquals(status, HttpStatus.ERROR_IN_PARAMETERS);
	}
	
	
	
}
