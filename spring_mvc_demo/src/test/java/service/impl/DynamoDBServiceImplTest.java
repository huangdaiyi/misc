package service.impl;

import java.util.List;

import model.CellPhone;
import model.RequestBase;
import model.SSOToken;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import service.DynamoDBService;
import base.BaseTestCase;

public class DynamoDBServiceImplTest extends BaseTestCase {

	@Autowired
	private DynamoDBService dynamoDBService;

	@Test
	public void testCheckToken() {
		RequestBase requestBase = dynamoDBService.checkToken("37d4d426978dfd738a8868ece34c57d9f851d8ea");
		Assert.assertEquals("73450575e5c049baf03e53d3540edd64", requestBase.getUserId());
		Assert.assertEquals("mydevice", requestBase.getDeviceId());
	}

	@Test
	public void testGetSSOToken() {
		SSOToken token = dynamoDBService.getSSOToken("37d4d426978dfd738a8868ece34c57d9f851d8ea");
		Assert.assertEquals("73450575e5c049baf03e53d3540edd64", token.getUserId());
		Assert.assertEquals("mydevice", token.getDeviceId());
	}

	@Test
	public void testFindCellphonesByUserId() {
		List<CellPhone> cellphones = dynamoDBService.findCellphonesByUserId("73450575e5c049baf03e53d3540edd64");
		Assert.assertEquals(1, cellphones.size());
		Assert.assertEquals("886", cellphones.get(0).getCountryCode());
		Assert.assertEquals("0919180470", cellphones.get(0).getCellphone());
	}
}
