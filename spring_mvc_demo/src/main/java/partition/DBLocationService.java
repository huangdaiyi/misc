package partition;

import java.util.List;

import model.AvailableDB;
import model.DBLocation;
import exception.MetadataException;

public interface DBLocationService {

	public DBLocation locateDB(String userId);

	public DBLocation residentDB(String userId, String serverName) throws MetadataException;

	public AvailableDB pickupAvailableDB(List<AvailableDB> databases, boolean allowExceed);

}
