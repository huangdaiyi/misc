package model;

import java.util.Calendar;

public class UserBlock {
	private Object block = new Object();
	private Calendar date = Calendar.getInstance();
	
	public Object getBlock() {
		return block;
	}

	public Calendar getDate() {
		return date;
	}
	public void setDate(Calendar date) {
		this.date = date;
	}
	
	
}
