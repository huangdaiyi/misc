package model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.base.Splitter;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Mail implements Serializable {

	private static final long serialVersionUID = 1L;

	private String mail;
	@JsonProperty("mail_status_code")
	private Integer mailStatusCode = null;

	public Mail() {
		super();
	}

	public Mail(String mail) {
		super();
		this.mail = mail;
	}

	public boolean isMailEquals(Mail compareTo) {
		if (compareTo == null) {
			return false;
		} else if (mail == null) {
			return false;
		}
		return mail.equalsIgnoreCase(compareTo.getMail());
	}

	public static List<Mail> toMails(String mails) {
		List<Mail> result = new ArrayList<Mail>();
		for (String mail : Splitter.on("/").trimResults().split(mails)) {
			result.add(new Mail(mail));
		}
		return result;
	}

	@Override
	public String toString() {
		return mail;
	}

	public String getMail() {
		return mail;
	}

	public void setMail(String mail) {
		this.mail = mail;
	}

	public Integer getMailStatusCode() {
		return mailStatusCode;
	}

	public void setMailStatusCode(Integer mailStatusCode) {
		this.mailStatusCode = mailStatusCode;
	}

}