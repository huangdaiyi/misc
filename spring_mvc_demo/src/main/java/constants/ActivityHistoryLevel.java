package constants;

public enum ActivityHistoryLevel {
	NONE(""),
	SYSTEM("system"),
	NORMAL("normal");
	
	private String level;
	
	private ActivityHistoryLevel(String level){
		this.level = level;
	}
	
	public void setLevel(String level){
		this.level = level;
	}
	
	public String getLevel(){
		return this.level.toLowerCase();
	}

}
