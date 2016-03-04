package custumer.test;

public class MyTree {

	private String id;
	private String pId;
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getpId() {
		return pId;
	}
	
	public void setpId(String pId) {
		this.pId = pId;
	}
	
	public MyTree(String id, String pId) {
		super();
		this.id = id;
		this.pId = pId;
	}
	@Override
	public String toString() {
		
		return String.format("id ---> %s; pid ---> %s", id, pId);
	}
	
	
	
	
	
	
}

