package model;

public class SharedRootMap {
	
	private String sharedRootId ;
	private String id;
	
	public String getSharedRootId() {
		return sharedRootId;
	}
	
	
	public SharedRootMap(String sharedRootId, String id) {
		super();
		this.sharedRootId = sharedRootId;
		this.id = id;
	}

	public void setSharedRootId(String sharedRootId) {
		this.sharedRootId = sharedRootId;
	}
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	
	

}
