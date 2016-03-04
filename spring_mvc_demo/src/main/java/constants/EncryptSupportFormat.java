package constants;

public enum EncryptSupportFormat {
	PDF("pdf"),
	DOC("doc"), DOCX("docx"),
	XLS("xls"), XLSX("xlsx"), CSV("csv"),
	PPT("ppt"), PPTX("pptx"),
	TXT("txt"),
	PNG("png"), JPG("jpg"), GIF("gif"), JPEG("jpeg"), BMP("bmp");
	
	private String format;
	
	private EncryptSupportFormat(String format) {
		this.format = format;
	}
	
	@Override
	public String toString() {
		return this.format;
	}
}
