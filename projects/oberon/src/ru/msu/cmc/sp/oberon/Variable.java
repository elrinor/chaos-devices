package ru.msu.cmc.sp.oberon;

import java.util.*;
import antlr.Token;
import antlr.TokenStreamException;
import antlr.TokenStreamRecognitionException;

public class Variable {
	public Type type;
	public String name;
	public Stack<TypedValue> values;
	public int definitionLine;
	public int definitionCol;
	public boolean isConst;
	public int id;

	public String toString() {
		StringBuffer result = new StringBuffer("");
		result.append(id + ": ");
		if(isConst)
			result.append("CONST ");
		result.append(name + ": " + type.toString());
		if(isConst)
			result.append(" = " + value().toString());
		return result.toString();
	}

	
	public TypedValue value() {
		return values.lastElement();
	}
	
	protected static int unnamedNumber = 0;
	protected static String newUnnamed() {
		return "Unnamed" + unnamedNumber++;
	}
	
	public Variable(String name, Type type, Object value, int definitionLine, int definitionCol, boolean isConst) {
		this.values = new Stack<TypedValue>();
		this.name = name;
		this.type = type;
		this.values.add(new TypedValue(type, value));
		this.definitionLine = definitionLine;
		this.definitionCol = definitionCol;
		this.isConst = isConst;
	}
	public Variable(Token nameToken, Type type, Object value, boolean isConst) {
		this(nameToken.getText(), type, value, nameToken.getLine(), nameToken.getColumn(), isConst);
	}
	public Variable(Token nameToken, TypedValue value, boolean isConst) {
		this(nameToken, value.type, value.value, isConst);
	}
	public Variable(Token nameToken, BuiltInType builtInType, Object value, boolean isConst) {
		this(nameToken.getText(), new Type(builtInType), value, nameToken.getLine(), nameToken.getColumn(), isConst);
	}
	public Variable(BuiltInType builtInType, Object value, boolean isConst) {
		this(newUnnamed(), new Type(builtInType), value, -1, -1, isConst);
	}
	public Variable(Type type, Object value, boolean isConst) {
		this(newUnnamed(), type, value, -1, -1, isConst);
	}
	
}
