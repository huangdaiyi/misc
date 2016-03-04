package factory.impl;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.util.concurrent.ThreadFactoryBuilder;

@Component
public class ThreadPool extends ThreadPoolExecutor {

	@Value("${thread.pool.max}")
	private Integer threadPoolMaxSize;

	public ThreadPool() {
		super(0, 1, 60, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>(), new ThreadFactoryBuilder().setDaemon(true).build());
		allowCoreThreadTimeOut(true);
	}

	@PostConstruct
	private void init() {
		this.setCorePoolSize(threadPoolMaxSize);
		this.setMaximumPoolSize(threadPoolMaxSize);
	}

}
