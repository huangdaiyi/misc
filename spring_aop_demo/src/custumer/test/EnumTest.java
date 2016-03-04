package custumer.test;

public enum EnumTest {
	Hardy("hardy"), Tom("tom"), Cat("act");

	private String name;

	private EnumTest(String name) {
		this.name = name;
	}
	
	public static EnumTest parse(String name) {
		//return new EnumTest(name);
		//EnumTest.v
		 return EnumTest.valueOf(name);
	}
	
	
	@Override
	public String toString() {
		return this.name;
	}
}
