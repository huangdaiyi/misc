package model;

public class ItemStyle {

	private String itemTextColor;
	private String itemBgColor;
	private Boolean itemTextBold;
	private Boolean itemTextItalic;
	private Boolean itemTextUnderline;
	
	public ItemStyle() {
		super();
	}

	public ItemStyle(String itemTextColor, String itemBgColor,
			Boolean itemTextBold, Boolean itemTextItalic,
			Boolean itemTextUnderline) {
		super();
		this.itemTextColor = itemTextColor;
		this.itemBgColor = itemBgColor;
		this.itemTextBold = itemTextBold;
		this.itemTextItalic = itemTextItalic;
		this.itemTextUnderline = itemTextUnderline;
	}

	public static ItemStyle fromMetadata(Metadata metadata) {
		return new ItemStyle(metadata.getItemTextColor(),
				metadata.getItemBgColor(),
				metadata.isItemTextBold(),
				metadata.isItemTextItalic(),
				metadata.isItemTextUnderline());
	}
	
	public String getItemTextColor() {
		return itemTextColor;
	}
	public void setItemTextColor(String itemTextColor) {
		this.itemTextColor = itemTextColor;
	}
	public String getItemBgColor() {
		return itemBgColor;
	}
	public void setItemBgColor(String itemBgColor) {
		this.itemBgColor = itemBgColor;
	}
	public Boolean isItemTextBold() {
		return itemTextBold;
	}
	public void setItemTextBold(Boolean itemTextBold) {
		this.itemTextBold = itemTextBold;
	}
	public Boolean isItemTextItalic() {
		return itemTextItalic;
	}
	public void setItemTextItalic(Boolean itemTextItalic) {
		this.itemTextItalic = itemTextItalic;
	}
	public Boolean isItemTextUnderline() {
		return itemTextUnderline;
	}
	public void setItemTextUnderline(Boolean itemTextUnderline) {
		this.itemTextUnderline = itemTextUnderline;
	}
}
