package partition.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import model.AvailableDB;
import model.DBLocation;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import partition.DBLocationService;
import service.impl.DynamoDBServiceImpl;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.AttributeValueUpdate;
import com.amazonaws.services.dynamodbv2.model.GetItemRequest;
import com.amazonaws.services.dynamodbv2.model.GetItemResult;
import com.amazonaws.services.dynamodbv2.model.ScanRequest;
import com.amazonaws.services.dynamodbv2.model.ScanResult;
import com.amazonaws.services.dynamodbv2.model.UpdateItemRequest;

import exception.MetadataException;

@Service
public class DBLocationServiceImpl implements DBLocationService {
	private static final Logger logger = LogManager.getLogger(DynamoDBServiceImpl.class);

	@Value("${dblocation.locationdb}")
	private String locationdb;
	@Value("${dblocation.availabledb}")
	private String availabledb;

	@Autowired
	private AmazonDynamoDBClient dynamoDBClient;

	@Override
	public DBLocation locateDB(String userId) {
		Map<String, AttributeValue> key = new HashMap<String, AttributeValue>();
		key.put("user_id", new AttributeValue().withS(userId));
		GetItemRequest getItemRequest = new GetItemRequest()
				.withTableName(this.locationdb)
				.withKey(key)
				.withAttributesToGet(
						Arrays.asList("user_id", "db_host", "db_port", "server_name", "db_instance"));

		GetItemResult result = dynamoDBClient.getItem(getItemRequest);
		// Check the response.
		DBLocation location = new DBLocation();
		
		Map<String, AttributeValue> items = result.getItem();
		if (items != null){
			location.setUserId(userId);
			location.setHost(items.get("db_host").getS());
			location.setServerName(items.get("server_name").getS());
			location.setPort(Integer.parseInt(items.get("db_port").getN()));
			location.setDbInstance(items.get("db_instance").getS());
			return location;
		}
		return null;
	}

	@Override
	public DBLocation residentDB(String userId, String serverName) throws MetadataException{
		// TODO Uncompleted
		ScanRequest request = new ScanRequest()
			.withTableName(availabledb)
			.withAttributesToGet(Arrays.asList("id", "host", "port", "region", "capacity", "count", "instance"));
		ScanResult result = dynamoDBClient.scan(request);
		List<AvailableDB> dbs = new ArrayList<AvailableDB>();
		if (result.getCount() > 0) {
			for (int i = 0; i < result.getCount(); ++i){
				dbs.add(parseAvailableDB(result.getItems().get(i)));
			}
			AvailableDB currentDB = pickupAvailableDB(dbs, true);
			currentDB.setCount(currentDB.getCount() + 1);
			DBLocation location = new DBLocation(userId, currentDB.getHost(), currentDB.getPort(), serverName, currentDB.getInstance());

			dynamoDBClient.putItem(this.locationdb, buildLocationRequest(location));
			dynamoDBClient.updateItem(buildAvailableRequest(this.availabledb, currentDB));
			
			return location;
		}
		return null;
	}

	@Override
	public AvailableDB pickupAvailableDB(List<AvailableDB> databases, boolean allowExceed) {
		if (databases == null || databases.isEmpty()) {
			return null;
		}
		// filter available
		List<AvailableDB> availableDBs = new ArrayList<AvailableDB>();
		for (AvailableDB database : databases) {
			if (database.getCount() < database.getCapacity()) {
				availableDBs.add(database);
			}
		}
		if (allowExceed && availableDBs.isEmpty()) {
			logger.warn("no available database");
			availableDBs.addAll(databases);
		}
		if (availableDBs.isEmpty()) {
			return null;
		}
		// sort by size ratio
		Collections.sort(availableDBs, new Comparator<AvailableDB>() {
			@Override
			public int compare(AvailableDB db1, AvailableDB db2) {
				return Double.valueOf((double) db1.getCount() / db1.getCapacity()).compareTo((double) db2.getCount() / db2.getCapacity());
			}
		});
		int resultSize = Math.min(availableDBs.size(), 3);
		return availableDBs.subList(0, resultSize).get(new Random().nextInt(resultSize));
	}

	private AvailableDB parseAvailableDB(Map<String, AttributeValue> attributes){
		AvailableDB db = new AvailableDB();
		db.setId(attributes.get("id").getS());
		db.setHost(attributes.get("host").getS());
		db.setPort(Integer.parseInt(attributes.get("port").getN()));
		db.setRegion(attributes.get("region").getS());
		db.setCapacity(Integer.parseInt(attributes.get("capacity").getN()));
		db.setCount(Integer.parseInt(attributes.get("count").getN()));
		db.setInstance(attributes.get("instance").getS());
		return db;
	}

	private UpdateItemRequest buildAvailableRequest(String tableName, AvailableDB db){
		Map<String, AttributeValue> key = new HashMap<String, AttributeValue>();
		key.put("id", new AttributeValue().withS(db.getId()));

		Map<String, AttributeValueUpdate> attributes = new HashMap<String, AttributeValueUpdate>();
		attributes.put("count", new AttributeValueUpdate(new AttributeValue().withN(String.valueOf(db.getCount())), "PUT"));

		UpdateItemRequest updateItemRequest = new UpdateItemRequest()
			.withTableName(tableName)
			.withKey(key)
			.withAttributeUpdates(attributes);
		return updateItemRequest;
	}

	private Map<String, AttributeValue> buildLocationRequest(DBLocation location){
		Map<String, AttributeValue> map = new HashMap<String, AttributeValue>();
		map.put("user_id", new AttributeValue().withS(location.getUserId()));
		map.put("db_host", new AttributeValue().withS(location.getHost()));
		map.put("db_port", new AttributeValue().withN(String.valueOf(location.getPort())));
		map.put("server_name", new AttributeValue().withS(location.getServerName()));
		map.put("db_instance", new AttributeValue().withS(location.getDbInstance()));
		return map;
	}

}
