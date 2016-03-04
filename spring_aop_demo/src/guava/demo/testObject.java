package guava.demo;

import java.util.ArrayList;
import java.util.List;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public class testObject {
	public static void main(String[] args) {
		List<BackupResult> backupResults = new ArrayList<BackupResult>();
		final String metadataId = "1111111111";
		backupResults.add(new BackupResult(0, 0, 1, metadataId, "1", false));
		backupResults.add(new BackupResult(0, 0, 2, metadataId, "2", false));
		backupResults.add(new BackupResult(0, 0, 3, metadataId, "3", false));
		backupResults.add(new BackupResult(0, 0, 4, metadataId, "4", false));
		backupResults.add(new BackupResult(0, 0, 5, metadataId, "5", false));
		backupResults.add(new BackupResult(0, 0, 6, metadataId, "6", false));
		/*Iterables.find(backupResults, new Predicate<BackupResult>() {

			@Override
			public boolean apply(BackupResult input) {
				return input.getMetadataId() == metadataId;
			}

		}).addSize(1000);*/

		Iterable<Long> tempSizes = Iterables.transform(backupResults,
				new Function<BackupResult, Long>() {

					@Override
					public Long apply(BackupResult input) {
						// TODO Auto-generated method stub
						return input.getSize();
					}

				});

		List<Long> longsLists = Lists.newArrayList(tempSizes);
		
		List<Long> sub = longsLists.subList(3, 6);
		
//		for (BackupResult backupResult : backupResults) {
//			System.out.println(backupResult.getSize());
//		}
		
		for (Long l : sub) {
			System.out.println(l);
		}
		
		
		

	}
}
