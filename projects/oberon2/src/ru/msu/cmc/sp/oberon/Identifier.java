package ru.msu.cmc.sp.oberon;

import java.util.Stack;

public class Identifier {
	private Type type;
	private Stack<Object> values;
	private boolean isConst;
	private int no;
	public int defLine;
	public int defCol;
	public String name;
	public int usages;
	
	public int getDefLine() {
		return defLine;
	}
	public int getDefCol() {
		return defCol;
	}
	public void setNo(int number) {
		this.no = number;
	}
	public int getNo() {
		return no;
	}
	public boolean isConst() {
		return isConst;
	}
	public Type getType() {
		return type;
	}
	public void setType(Type type) {
		this.type = type;
	}
	public Object getValue() {
		return values.lastElement();
	}
	public void setValue(Object value) {
		values.pop();
		values.push(value);
	}
	public void pushValue(Object value) {
		values.push(value);
	}
	public void popValue() {
		values.pop();
	}
	public void pushDefaultValue() {
		values.push(type.defaultValue());
	}
	public Identifier(Type type, Object value, boolean isConst, int defLine, int defCol) {
		this.values = new Stack<Object>();
		this.isConst = isConst;
		this.type = type;
		this.defLine = defLine;
		this.defCol = defCol;
		if(isConst)
			this.values.push(value);
		this.usages = 0;
		this.name = "";
	}
	public Identifier(Type type) {
		this(type, null, false, 0, 0);
	}
	public Identifier(Type type, Object value) {
		this(type, value, true, 0, 0);
	}
}
