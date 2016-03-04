package factory;

public interface AWSClientFactory<T> {

	public T createClient();

}
