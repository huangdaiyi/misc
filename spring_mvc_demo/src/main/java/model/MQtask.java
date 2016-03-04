package model;

import java.util.List;

public class MQtask {

	private List<Metadata> readerTask;
	private List<Metadata> streamTask;

	public List<Metadata> getReaderTask() {
		return readerTask;
	}

	public void setReaderTask(List<Metadata> readerTask) {
		this.readerTask = readerTask;
	}

	public List<Metadata> getStreamTask() {
		return streamTask;
	}

	public void setStreamTask(List<Metadata> streamTask) {
		this.streamTask = streamTask;
	}

}
