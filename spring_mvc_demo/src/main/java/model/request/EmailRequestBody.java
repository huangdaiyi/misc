package model.request;

import java.util.Map;

import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class EmailRequestBody extends RequestBase {

	private static final long serialVersionUID = 1;
	@JsonProperty("From")
	private String from;
	@JsonProperty("To")
	private String to;
	@JsonProperty("Subject")
	private String subject;
	@JsonProperty("Body")
	private String body;
	@JsonProperty("IsNeedLog")
	private String needLog;
	@JsonProperty("Priority")
	private String priority;
	@JsonProperty("ContentType")
	private String contentType;
	@JsonProperty("MailType")
	private String mailType;
	@JsonProperty("SmtpSetting")
	private Map<String, String> smtpSetting;

	public EmailRequestBody() {
		super();
	}

	public String getFrom() {
		return from;
	}

	public void setFrom(String from) {
		this.from = from;
	}

	public String getTo() {
		return to;
	}

	public void setTo(String to) {
		this.to = to;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getBody() {
		return body;
	}

	public void setBody(String body) {
		this.body = body;
	}

	public String getNeedLog() {
		return needLog;
	}

	public void setNeedLog(String needLog) {
		this.needLog = needLog;
	}

	public String getPriority() {
		return priority;
	}

	public void setPriority(String priority) {
		this.priority = priority;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getMailType() {
		return mailType;
	}

	public void setMailType(String mailType) {
		this.mailType = mailType;
	}

	public Map<String, String> getSmtpSetting() {
		return smtpSetting;
	}

	public void setSmtpSetting(Map<String, String> smtpSetting) {
		this.smtpSetting = smtpSetting;
	}

}
