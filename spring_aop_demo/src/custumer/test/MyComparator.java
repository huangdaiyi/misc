package custumer.test;

import java.util.Comparator;

public class MyComparator implements Comparator<MyTree> {

	@Override
	public int compare(MyTree t1, MyTree t2) {
		if (t1.getpId().equals("")||t1.getId().equals(t2.getpId())) {
			return 1;
		}else if(t1.getpId().equals(t2.getpId())) {
			return 0;
		}else if(t1.getpId().equals(t2.getId())){
			return -1;
		}
		
		return 0;
	}
}