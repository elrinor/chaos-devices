package ru.msu.cmc.sp.oberon;

//What to Store as Value?
//Array - ArrayList<TypedValue>
//Procedure - CodeBlock
//Module - ?

public enum BuiltInType {
	VOID     (false, false, false, 0),
	STRING   (false, false, false, 0),
	BOOLEAN  (true,  false, false, 0),
	CHAR     (true,  false, false, 0),
	SHORTINT (true,  true,  true,  1),
	INTEGER  (true,  true,  true,  2),
	LONGINT  (true,  true,  true,  3),
	REAL     (true,  true,  false, 4),
	LONGREAL (true,  true,  false, 5),
	ARRAY    (false, false, false, 0),
	PROCEDURE(false, false, false, 0);
	
	private boolean isNumeric;
	private boolean isSimple;
	private boolean isInt;
	private int typeWidth;
	public boolean isSimple() {
		return isSimple;
	}
	public boolean isNumeric() {
		return isNumeric;
	}
	public boolean isInt() {
		return isInt;
	}
	public boolean isFloat() {
		return isNumeric && !isInt;
	}
	public int typeWidth() {
		return typeWidth;
	}
	public boolean canBeConst() { // can it be a const in an expression?
		return isSimple || this == STRING;
	}
	private BuiltInType(boolean isSimple, boolean isNumeric, boolean isInt, int typeWidth) {
		this.isSimple = isSimple;
		this.isNumeric = isNumeric;
		this.isInt = isInt;
		this.typeWidth = typeWidth;
	}
}

