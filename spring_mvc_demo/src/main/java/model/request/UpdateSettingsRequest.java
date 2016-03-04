package model.request;

import model.RequestBase;

public class UpdateSettingsRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	private String settings;

	public UpdateSettingsRequest() {
		super();
	}

	public String getSettings() {
		return settings;
	}

	public void setSettings(String settings) {
		this.settings = settings;
	}

}
