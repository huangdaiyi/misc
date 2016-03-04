package custumer.test;
class Out {
    private int age = 12;
 
    public static void main(String[] args) {
    	new Out().Print(123);
	}
    
    public void Print(final int x) {
        class In {
            public void inPrint() {
                System.out.println(x);
                System.out.println(age);
            }
        }
        new In().inPrint();
    }
}