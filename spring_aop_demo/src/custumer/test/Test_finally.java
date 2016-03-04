package custumer.test;

public class Test_finally {
	
	public static void main(String[] args) {
		int x = add();
		System.out.println(x);
	}

	
	private static int add() {
		int x = 1;
		try {
			return ++x;
		} catch (Exception e) {
			
		}finally{
			 ++x;
		}
		return x;
	}
}
