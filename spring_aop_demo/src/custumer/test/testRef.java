package custumer.test;


public class testRef {
	
	
	public static void main(String[] args) {
		BaseStop stop = new BaseStop();
		
		BaseStop stop2 = stop;
		
		
		stop.checkStop();
		stop2.checkStop();
		
		stop2.setStop(true);
		
		stop.checkStop();
		stop2.checkStop();
		
	}
	
}
