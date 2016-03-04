package dao;

import java.util.List;

import model.SourceTargetMap;
import model.TextViewSetting;


public interface TextViewSettingDao {

	public TextViewSetting getTextViewSetting(String metadataIndexId, String viewerId, String viewerDeviceUniqueId);
	
	public void updateTextViewSetting(TextViewSetting textViewSetting);
	
	public void updateAllBackupsTextViewSetting(TextViewSetting textViewSetting);

	public void copyTextViewSetting(String sourceId, String targetId);

	public void copyTextViewSettings(List<SourceTargetMap> maps); 
}
