package model.request;

import java.util.List;

import model.DefaultBackupSetting;
import model.RequestBase;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UpdateExtensionBackupRequest extends RequestBase {

	private static final long serialVersionUID = 1L;

	@JsonProperty("setting")
	private List<DefaultBackupSetting> defaultBackupSettings;

	public List<DefaultBackupSetting> getDefaultBackupSettings() {
		return defaultBackupSettings;
	}

	public void setDefaultBackupSettings(
			List<DefaultBackupSetting> defaultBackupSettings) {
		this.defaultBackupSettings = defaultBackupSettings;
	}

	

}
