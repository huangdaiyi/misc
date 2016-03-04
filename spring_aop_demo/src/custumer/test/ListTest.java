package custumer.test;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ListTest {  
    public static void main(String[] args) {  
         testUnion();  
        // testIntersectionSet();  
    }  

    @SuppressWarnings("unused")
	private static void testIntersectionSet() {  
         List<String> list1 = new ArrayList<String>();  
         List<String> list2 = new ArrayList<String>();  
         list1.add("abc");  list2.add("abc");  
         list1.add("123");  list2.add("123");  
         list1.add("ABC");  
         list2.add("XYZ");  
         Set<String> set =new HashSet<String>(list1);  
         set.addAll(list2);
         System.out.println("并集——————————————————————————");  
         for (String str : set) {
        	 System.out.println("并集元素个数是：" + str);  
		}
               
  
    }  
    private static void testUnion() {  
         List<String> list1 = new ArrayList<String>();  
         List<String> list2 = new ArrayList<String>();  
         list1.add("abc");  list2.add("abc");  
         list1.add("123");  list2.add("123");  
         list1.add("ABC");  list2.add("XYZ");  
         list1.add("111");  list2.add("222");  
         
         for (int i = 0; i < 10000; i++) {
			list1.add(String.valueOf(i));
			list2.add(String.valueOf(i));
		}
         //list1.retainAll(list2);  
         Set<String> set =new HashSet<String>(list1);  
         //set.retainAll(list2);
         set.removeAll(list2);
         System.out.println("差集——————————————————————————");  
         //System.err.println();
         for (String str : set) {
  
        	 System.out.println("并集元素个数是：" + str);  
		}
               
         
        // System.out.println("交集元素个数是："+list1.size());  
    }  
//    private static void addList2Set(Set<String> set,List<String> list){  
//        for (String str : list) {  
//            set.add(str);  
//        }  
//    }  
} 