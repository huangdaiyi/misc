package custumer.test;

public class BaseStop {
	private boolean stop= false;

	public boolean isStop() {
		return stop;
	}

	public void setStop(boolean stop) {
		this.stop = stop;
	}
	
	
	public void checkStop(){
		if (stop) {
			System.out.println("stoped");
		}
		else {
			System.out.println("running");
		}
	}
	
}
