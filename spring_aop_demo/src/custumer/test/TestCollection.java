package custumer.test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;

public class TestCollection {
	public static void main(String[] args) {

//		String id = getUUID();
//		String pid = "";
		List<MyTree> ts = new ArrayList<MyTree>();
		// //TreeMap<String, String> t = new TreeMap<String, String>();
		// for (int i = 0; i <10; i++) {
		// ts.add(new MyTree(id, pid));
		// pid = id;
		// id = getUUID();
		// }
		//
		// System.out.println(ts);

		ts.add(new MyTree("1", ""));
		ts.add(new MyTree("2", "1"));
		ts.add(new MyTree("3", "2"));
		ts.add(new MyTree("4", "1"));
		ts.add(new MyTree("5", "4"));
		ts.add(new MyTree("6", "5"));
		ts.add(new MyTree("7", "2"));
		ts.add(new MyTree("8", "2"));
		ts.add(new MyTree("9", "3"));
		ts.add(new MyTree("10", "4"));

		TreeSet<MyTree> treeSet = new TreeSet<MyTree>(new MyComparator());
		treeSet.addAll(ts);
		for (MyTree t : treeSet) {
			System.out.println(t.toString());
		}
		
		List<String> test =  new ArrayList<String>();
		test.add("1");
		test.add("2");
		test.add("3");
		test.add("4");
		
		for (String string : test.subList(0, 1)) {
			System.out.println(string);
		}

	}

	public static String getUUID() {
		return UUID.randomUUID().toString();
	}

}