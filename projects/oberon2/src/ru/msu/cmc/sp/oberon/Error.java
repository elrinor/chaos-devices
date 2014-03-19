package ru.msu.cmc.sp.oberon;

import antlr.RecognitionException;

public class Error extends RecognitionException{
	public static class Break extends Error {
		private static final long serialVersionUID = -8356738098436045771L;
	}
	public static class Continue extends Error {
		private static final long serialVersionUID = 7519455425300101367L;
	}
	public static class Return extends Error {
		private static final long serialVersionUID = -1455261979111703076L;
		private Object value;
		public Object getValue() {
			return value;
		}
		public Return(Object value) {
			this.value = value;
		}
	}
	public static class Halt extends Error {
		private static final long serialVersionUID = -6322565019942166665L;
		private int code;
		public int getCode() {
			return code;
		}
		public Halt(int code) {
			this.code = code;
		}
	}
	
	private static final long serialVersionUID = -541649880930153809L;
	private int errorCode;
	
	public int errorCode() {
		return errorCode;
	}

	public static final int COMPILEERROR = 100;
	public static final int INPUTERROR = 101;
	public static final int ZERODIVIDE = 102;
	public static final int INTOVERFLOW = 103;
	public static final int DOMAINERROR = 104;
	public static final int OUTOFBOUNDS = 105;

	public static String defaultMessage(int errorCode) {
		switch (errorCode) {
		case COMPILEERROR:
			return "Parser/Lexer error";
		case INPUTERROR:
			return "Input format error";
		case ZERODIVIDE:
			return "Division by 0";
		case INTOVERFLOW:
			return "Integer overflow";
		case DOMAINERROR:
			return "Domain error";
		case OUTOFBOUNDS:
			return "Index out of bounds";
		default:
			return "Unknown error";
		}
	}

	public Error(int errorCode) {
		super(defaultMessage(errorCode));
		this.errorCode = errorCode;
	}

	public Error(int errorCode, String message) {
		super(message);
		this.errorCode = errorCode;
	}
	
	public Error() {
		super();
		this.errorCode = 0;
	}
	
	static public void Break() throws Break {
		throw new Error.Break();
	}
}
