package utils;

public class MathUtils {

	public static int ceil(int numberA, double numberB) {
		return (int) Math.ceil(numberA / numberB);
	}

	public static int ceil(int numberA, double numberB, int defaultValue) {
		if (numberB == 0) {
			return defaultValue;
		}
		return (int) Math.ceil(numberA / numberB);
	}

	public static long toPositive(long number) {
		return (number >= 0 ? number : 0);
	}

}
