package custumer.test;
public  class A implements Cloneable{
		private static A aa = null;
		public static A CreateA(){
			aa = new A();
			return aa;
		}
		
		private String a ="";

		public String getA() {
			return a;
		}

		public void setA(String a) {
			this.a = a;
		}
		
		@Override
		public Object clone(){
			
			try {
				return super.clone();
			} catch (CloneNotSupportedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			return null;
		}
				
	}