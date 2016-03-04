package utils;

import java.util.ArrayList;
import java.util.List;

import model.CellPhone;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;

public final class CellphonesUtils {
	public static String toDisplayCellphone(List<CellPhone> cellphones){
		StringBuilder displayCellphone = new StringBuilder();
		for(CellPhone cellphone : cellphones){			
			if(displayCellphone.length()!=0){
				displayCellphone.append("/");
			}
			// eliminate 0
			String cellphoneNumber = String.valueOf(Integer.parseInt(cellphone.getCellphone()));
			String countryCode = cellphone.getCountryCode();
			displayCellphone = displayCellphone.append("(+").append(countryCode)
					.append(")").append(cellphoneNumber);
		}
		return displayCellphone.toString();
	}
	
	public static String composeCellphones(List<CellPhone> cellphones) {
		return Joiner.on("/").join(cellphones);
	}
	
	public static List<CellPhone> buildCellphones(String cellphones){
		List<CellPhone> cellPhones = new ArrayList<CellPhone>();
		if (StringUtils.isNullOrEmpty(cellphones)) {
			return cellPhones;
		}
		
		List<String> cellphoneList = Splitter.onPattern("\\/|,").trimResults().splitToList(cellphones);
		for (int j = 0; j < cellphoneList.size(); j++) {
			CellPhone cellPhoneObj = new CellPhone();
			cellPhoneObj.setCellphone(cellphoneList.get(j));
			cellPhoneObj.setCountryCode(cellphoneList.get(++j));
			cellPhones.add(cellPhoneObj);
		}
		return cellPhones;
	}
	public static boolean compareCellphones(String fullCellphones,String targetCellphone){
		List<String> fullCellphonesList = Splitter.onPattern("\\/").trimResults().splitToList(fullCellphones);
		List<String> targetCellphonesList = Splitter.onPattern("\\/").trimResults().splitToList(targetCellphone);
		for (String cellphone : fullCellphonesList) {
			if (targetCellphonesList.contains(cellphone)) {
				return true;
			}
		}
		
		return false;
	}
	
	public static boolean compareCellphones(List<CellPhone> fullCellphones,List<CellPhone> targetCellphone){
		return compareCellphones(composeCellphones(fullCellphones),composeCellphones(targetCellphone));
	}
}
