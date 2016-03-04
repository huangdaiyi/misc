package factory.impl;

import java.util.concurrent.CountDownLatch;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.amazonaws.services.sqs.AmazonSQSClient;
import com.amazonaws.services.sqs.model.SendMessageBatchRequest;

public class MQMuitiThreadMessageSender implements Runnable {

	private static final Logger logger = LogManager
			.getLogger(MQMuitiThreadMessageSender.class);

	private final SendMessageBatchRequest request;
	private final AmazonSQSClient client;
	private CountDownLatch threadsSignal;

	public MQMuitiThreadMessageSender(SendMessageBatchRequest req,
			AmazonSQSClient client, CountDownLatch threadsSignal) {
		this.request = req;
		this.client = client;
		this.threadsSignal = threadsSignal;
	}

	@Override
	public void run() {
		try {
			client.sendMessageBatch(request);
		} catch (Exception e) {
			logger.error(String.format("batch send metadata Exception:%s",
					e.toString()));
		} finally {
			threadsSignal.countDown();
		}
	}

}