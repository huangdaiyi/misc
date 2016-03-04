package service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import model.ActiveHistory;
import model.CellPhone;
import model.RequestBase;
import model.SSOToken;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import service.DynamoDBService;
import utils.DateUtils;
import utils.StringUtils;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBQueryExpression;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.BatchWriteItemRequest;
import com.amazonaws.services.dynamodbv2.model.ComparisonOperator;
import com.amazonaws.services.dynamodbv2.model.Condition;
import com.amazonaws.services.dynamodbv2.model.GetItemRequest;
import com.amazonaws.services.dynamodbv2.model.GetItemResult;
import com.amazonaws.services.dynamodbv2.model.PutRequest;
import com.amazonaws.services.dynamodbv2.model.WriteRequest;

import factory.impl.ThreadPool;


@Service
public class DynamoDBServiceImpl implements DynamoDBService {

	private static final Logger logger = LogManager
			.getLogger(DynamoDBServiceImpl.class);

	@Autowired
	private AmazonDynamoDBClient dynamoDBClient = null;
	@Autowired
	private ThreadPool threadPool;

	public DynamoDBServiceImpl() {
		super();
	}

	@Override
	public RequestBase checkToken(String token) {
		SSOToken ssoToken = getSSOToken(token);
		if (ssoToken == null || ssoToken.getUserId().isEmpty()) {
			return null;
		}
		return ssoToken.toRequestBase();
	}

	@Override
	public SSOToken getSSOToken(String token) {
		try {
			Map<String, AttributeValue> key = new HashMap<String, AttributeValue>();
			key.put("token", new AttributeValue().withS(token));
			GetItemRequest getItemRequest = new GetItemRequest()
					.withTableName("sso-tokens")
					.withKey(key)
					.withAttributesToGet("user_id", "device_id");
			GetItemResult result = dynamoDBClient.getItem(getItemRequest);
			Map<String, AttributeValue> resultItem = result.getItem();
			if (resultItem != null) {
				SSOToken ssoToken = new SSOToken();
				for (Map.Entry<String, AttributeValue> item : resultItem
						.entrySet()) {
					String attributeName = item.getKey();
					AttributeValue value = item.getValue();
					if ("user_id".equals(attributeName)) {
						String userId = value.getS();
						ssoToken.setUserId(userId == null ? "" : userId);
					} else if ("device_id".equals(attributeName)) {
						String deviceId = value.getS();
						ssoToken.setDeviceId(deviceId == null ? "" : deviceId);
					}
				}
				return ssoToken;
			}
		} catch (AmazonServiceException ase) {
			logger.error("AmazonServiceException:{}", ase.toString());
		} catch (NullPointerException e) {
			logger.error("NullPointerException:{}", e.toString());
		}
		return null;
	}

	public List<CellPhone> findCellphonesByUserId(String userId) {
		List<CellPhone> cellphones = new ArrayList<CellPhone>();
		try {
			Map<String, AttributeValue> key = new HashMap<String, AttributeValue>();
			key.put("user_id", new AttributeValue().withS(userId));
			GetItemRequest getItemRequest = new GetItemRequest().withTableName("sso-users").withKey(key).withAttributesToGet("cellphones");
			GetItemResult result = dynamoDBClient.getItem(getItemRequest);
			Map<String, AttributeValue> resultItem = result.getItem();
			if (resultItem != null) {
				List<AttributeValue> cellphonesList = resultItem.get("cellphones").getL();
				for (AttributeValue cellphonesData : cellphonesList) {
					Map<String, AttributeValue> cellphoneData = cellphonesData.getM();
					CellPhone cellphone = new CellPhone();
					cellphone.setCellphone(cellphoneData.get("cellphone").getS());
					cellphone.setCountryCode(cellphoneData.get("country_code").getS());
					cellphones.add(cellphone);
				}
			}
		} catch (AmazonServiceException ase) {
			logger.error("AmazonServiceException:{}", ase.toString());
		} catch (NullPointerException e) {
			logger.error("NullPointerException:{}", e.toString());
		}
		return cellphones;
	}

	@Override
	public void createActivityHistory(List<ActiveHistory> historys) {
		Map<String, List<WriteRequest>> requestMap = new HashMap<String, List<WriteRequest>>(
				1);
		List<WriteRequest> list = new ArrayList<WriteRequest>(historys.size());
		for (ActiveHistory item : historys) {
			Map<String, AttributeValue> map = new HashMap<String, AttributeValue>(
					1);
			map.put("id",
					new AttributeValue().withS(String.valueOf(item.getId())));
			map.put("path", new AttributeValue().withS(item.getPath()));
			map.put("metadata_id",
					new AttributeValue().withS(item.getMetadataId()));
			map.put("last_edit_user",
					new AttributeValue().withS(item.getLastEditUser()));
			map.put("last_edit_time",
					new AttributeValue().withS(item.getLastEditTime()));
//			map.put("shared_root_id", new AttributeValue().withS(item.getSharedRootId()));
			map.put("change_reason", new AttributeValue().withS(item.getChangeReason()));
			map.put("level", new AttributeValue().withS(item.getLevel().toString()));

			WriteRequest writeRequest = new WriteRequest()
					.withPutRequest(new PutRequest().withItem(map));
			list.add(writeRequest);
		}
		requestMap.put("activity_history", list);
		BatchWriteItemRequest request = new BatchWriteItemRequest()
				.withRequestItems(requestMap);
		dynamoDBClient.batchWriteItem(request);

	}

	@Override
	public void createActivityHistory(final String fullPath, final String oldFullPath,
			final String sharedUpperPath, final String oldSharedUpperPath,
			final String metadataIndexId, final String lastEditUser, final String lastEditTime,
			final String changeReason, final String sharedRootId, final String level) {

		Thread thread = new Thread() {
			@Override
			public void run() {
				ActiveHistory history = new ActiveHistory();
				history.setId(UUID.randomUUID().toString());
				history.setPath(fullPath);
				history.setOldPath(oldFullPath);
				history.setSharedUpperPath(sharedUpperPath);
				history.setOldSharedUpperPath(oldSharedUpperPath);
				history.setMetadataId(metadataIndexId);
				history.setLastEditTime(lastEditTime);
				history.setLastEditUser(lastEditUser);
				history.setSharedRootId(sharedRootId);
				history.setChangeReason(changeReason);
				history.setLevel(level);	
				DynamoDBMapper mapper = new DynamoDBMapper(dynamoDBClient);
				mapper.save(history);
			}
		};
		threadPool.execute(thread);

	}
	@Override
	public  List<ActiveHistory> getActiveHistory(List<String> sharedRootIds,String toDate,String path,boolean isFolder) {
		DynamoDBMapper mapper = new DynamoDBMapper(dynamoDBClient);
		
		if (StringUtils.isNullOrEmpty(toDate)) {
			toDate = DateUtils.nowUTCDateTime();
		}else {
			toDate = toDate + " " +"23:59:59";
		}

		ActiveHistory activeHistory = new ActiveHistory();
		List<ActiveHistory> activeHistories = new ArrayList<ActiveHistory>();
		
		Condition rangeKeyCondition = new Condition();
		rangeKeyCondition.withComparisonOperator(ComparisonOperator.LE.toString())
	    .withAttributeValueList(new AttributeValue().withS(toDate)); 
		
		DynamoDBQueryExpression<ActiveHistory> queryExpression = new DynamoDBQueryExpression<ActiveHistory>()
	            .withHashKeyValues(activeHistory)
	            .withRangeKeyCondition("last_edit_time", rangeKeyCondition)
	            .withConsistentRead(false);
		
		if (!StringUtils.isNullOrEmpty(path)) {
			Condition pathCondition = new Condition();
			if (isFolder) {
				pathCondition.withComparisonOperator(ComparisonOperator.GE)
				 .withAttributeValueList(new AttributeValue().withS(path));
			}else{
				pathCondition.withComparisonOperator(ComparisonOperator.EQ)
				 .withAttributeValueList(new AttributeValue().withS(path));
			}
			Map<String, Condition> queryConditionsMap = new HashMap<String, Condition>();
			queryConditionsMap.put("path", pathCondition);
			
			queryExpression.withQueryFilter(queryConditionsMap);			
		}
		try {
			for (String  sharedRootId : sharedRootIds) {
				activeHistory.setSharedRootId(sharedRootId);
				queryExpression.withHashKeyValues(activeHistory);
				activeHistories.addAll(mapper.query(ActiveHistory.class, queryExpression));
			}			
		} catch (Exception e) {
			logger.error("AmazonQueryException:{}", e.toString());
		}
		
		return activeHistories;
	}
}
