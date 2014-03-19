package ru.msu.cmc.sp.oberon;

public enum Error {
	PARSEERROR       (100, "Parse error"),
	INPUTERROR       (101, "Input format error"),
	DIVISIONBYZERO   (102, "Division by 0"),
	INTEGEROVERFLOW  (103, "Integer overflow"),
	DOMAINERROR      (104, "Domain error"),
	OUTOFBOUNDS      (105, "Index out of bounds"),
	TYPEMISMATCH     (104, "Domain error");
	private int code;
	private String message;
	public int code() {
		return code;
	}
	public String message() {
		return message;
	}
	private Error(int code, String message) {
		this.code = code;
		this.message = message;
	}
}
