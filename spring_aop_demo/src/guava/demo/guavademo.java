package guava.demo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.base.Function;
import com.google.common.base.Preconditions;
import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public class guavademo {
	public static void main(String[] args) {
		List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

		if (Iterables.all(numbers, isPositive)) {
			System.out.println("Yep!");
		}

		Iterable<Integer> listIterables = Iterables.filter(numbers, isPositive);
		// Lists.newArrayList(listIterables);
		List<Integer> listsIntegers = Lists.newArrayList(listIterables);
		for (Integer integer : listIterables) {
			//System.out.println(integer);
			integer++;
		}
		for (Integer integer : numbers) {
			System.out.println(integer);
		}
		// Iterables.
		List<List<Integer>> sub = Lists.partition(listsIntegers, 3);

		System.out.println(sub.size());

		// Preconditions.checkArgument(expression, errorMessage);

		List<X> xList = new ArrayList<X>();
		xList.add(new X("a", "b", "v", "w", "m", "n"));
		xList.add(new X("a1", "b1", "v1", "w1", "m1", "n1"));
		for (X elem : xList) {
			System.out.println("An instance of X:" + elem);
		}

		Iterable<Y> trans = Iterables.transform(xList, new Function<X, Y>() {
			@Override
			public Y apply(X x) {
				return new Y(x.getA(), x.getB());
			}

		});
		
		for (Y y : trans) {
			System.out.println(y.getA() + ":" + y.getB());
		}
	}

	static boolean isEven(int num) {
		return (num % 2) == 0; // simple
	}

	static Predicate<Integer> isPositive = new Predicate<Integer>() {
		@Override
		public boolean apply(Integer number) {
			return number % 2 == 0;
		}
	};

}
