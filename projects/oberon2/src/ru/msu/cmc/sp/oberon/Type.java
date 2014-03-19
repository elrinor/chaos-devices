package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;

public class Type {
	public static final int SHORTINT = 1;
	public static final int INTEGER = 2;
	public static final int LONGINT = 3;
	public static final int REAL = 4;
	public static final int LONGREAL = 5;
	public static final int CHAR = 6;
	public static final int BOOLEAN = 7;
	public static final int STRING = 8;
	public static final int ARRAY = 9;
	public static final int FUNCTION = 10;
	public static final int TYPE = 11; // means that this var is typedef
	public static final int VOID = 0;

	// XXX
	// Note about values:
	// for array: ArrayList<Object>
	// for function: FunctionValue
	// for type: Type
	// fot void: null
	 
	public static Type Void = new Type(VOID);
	public static Type ShortInt = new Type(SHORTINT);
	public static Type Integer = new Type(INTEGER);
	public static Type LongInt = new Type(LONGINT);
	public static Type Real = new Type(REAL);
	public static Type LongReal = new Type(LONGREAL);
	public static Type Char = new Type(CHAR);
	public static Type Boolean = new Type(BOOLEAN);
	public static Type String = new Type(STRING);
	public static Type Type = new Type(TYPE);
	public static Type VoidFunction = Function(0, new ArrayList<Type>(), new ArrayList<Boolean>(), Void);
	//public static Type[] Types = new Type[] {Void, ShortInt, Integer, LongInt, Real, LongReal, Char, Boolean, String, null, null, Type};
	public static Type Array(int arraySize, Type arrayType) throws Error {
		if(arraySize < 0)
			throw new Error(Error.COMPILEERROR, "Cannot create array of size " + arraySize);
		Type array = new Type(ARRAY);
		array.addInfo = new Object[2];
		((Object[])array.addInfo)[0] = (Integer)arraySize;
		((Object[])array.addInfo)[1] = arrayType;
		return array;
	}
	public static Type Function(int funcParamCount, ArrayList<Type> funcParamType, ArrayList<Boolean> funcIsVarParam, Type funcReturnType) {
		Type func = new Type(FUNCTION);
		func.addInfo = new Object[4];
		((Object[])func.addInfo)[0] = (Integer)funcParamCount;
		((Object[])func.addInfo)[1] = funcParamType.toArray();
		((Object[])func.addInfo)[2] = funcIsVarParam.toArray();
		((Object[])func.addInfo)[3] = funcReturnType;
		return func;
	}
	public static Type Function(Type paramType, boolean isVarParam, Type returnType) {
		ArrayList<Type> funcParamType = new ArrayList<Type>();
		ArrayList<Boolean> funcIsVarParam = new ArrayList<Boolean>();
		funcParamType.add(paramType);
		funcIsVarParam.add(isVarParam);
		return Function(1, funcParamType, funcIsVarParam, returnType);
	}
	public static Type Function(Type returnType) {
		return Function(0, new ArrayList<Type>(), new ArrayList<Boolean>(), returnType);
	}
	public static Type Function(Type param0Type, Type param1Type, Type returnType) {
		ArrayList<Type> funcParamType = new ArrayList<Type>();
		ArrayList<Boolean> funcIsVarParam = new ArrayList<Boolean>();
		funcParamType.add(param0Type);
		funcParamType.add(param1Type);
		funcIsVarParam.add(false);
		funcIsVarParam.add(false);
		return Function(2, funcParamType, funcIsVarParam, returnType);
	}
	
	
	public Type(int typeId) {
		this.typeId = typeId;
	}
	
	public boolean isParametrized() {
		if (this.typeId == FUNCTION || this.typeId == ARRAY)
			return true;
		else
			return false;
	}
	
	public boolean isNumeric() {
		return (this.typeId >= SHORTINT && this.typeId <= LONGREAL);
	}
	
	public boolean isInteger() {
		return (this.typeId >= SHORTINT && this.typeId <= LONGINT);
	}
	
	private int typeId;
	private Object addInfo;
	
	public int typeId() {
		return typeId;
	}
	
	public int arraySize() {
		return (Integer)((Object[])addInfo)[0];
	}
	
	public Type arrayType() {
		return (Type)((Object[])addInfo)[1];
	}
	
	public int funcParamCount() {
		return (Integer)((Object[])addInfo)[0];
	}
	
	public Type funcParamType(int i) {
		return (Type)((Object[])((Object[])addInfo)[1])[i];
	}
	
	public boolean funcIsVarParam(int i) {
		return (Boolean)((Object[])((Object[])addInfo)[2])[i];
	}
	
	public Type funcReturnType() {
		return (Type)((Object[])addInfo)[3];
	}
	
	public boolean equals(Type t) {
		if(this.typeId != t.typeId)
			return false;
		if(!this.isParametrized())
			return true;
		if(this.typeId == ARRAY) {
			if((this.arraySize() == t.arraySize() || t.arraySize() == 0) && this.arrayType().equals(t.arrayType()))
				return true;
			else
				return false;
		}
		if(this.typeId == FUNCTION) {
			if(!this.funcReturnType().equals(t.funcReturnType()))
				return false;
			if(this.funcParamCount() != t.funcParamCount())
				return false;
			for(int i = 0; i < this.funcParamCount(); i++) {
				if(this.funcIsVarParam(i) != t.funcIsVarParam(i))
					return false;
				if(!this.funcParamType(i).equals(t.funcParamType(i)))
					return false;
			}
			return true;
		}
		return false;
	}
	
	public boolean canBeConvertedTo(Type t) {
		if(this.equals(t))
			return true;
		if(this.isNumeric() && t.isNumeric() && this.typeId <= t.typeId)
			return true;
		if(this.typeId == STRING && t.typeId == ARRAY && t.arrayType().typeId == CHAR)
			return true;
		if(this.typeId == ARRAY && this.arrayType().typeId == CHAR && t.typeId == STRING)
			return true;
		if(this.typeId == ARRAY && t.typeId == ARRAY && this.arrayType().equals(t.arrayType()) && t.arraySize() == 0)
			return true;
		if(this.typeId == CHAR && t.typeId == STRING)
			return true;
		return false;
	}
	
	public Object defaultValue() {
		switch (typeId) {
		case SHORTINT:
			return (Short)(short)0;
		case INTEGER:
			return (Integer)0;
		case LONGINT:
			return (Long)0l;
		case REAL:
			return (Float)0.0f;
		case LONGREAL:
			return (Double)0.0;
		case CHAR:
			return (Character)'\0';
		case BOOLEAN:
			return (Boolean)false;
		case STRING:
			return "";
		case TYPE:
			return null;
		case VOID:
			return null;
		case ARRAY:
			ArrayList<Object> tmp = new ArrayList<Object>(); 
			for(int i = 0; i < this.arraySize(); i++)
				tmp.add(this.arrayType().defaultValue());
			return tmp;
		case FUNCTION:
			return null;
		default:
			return null;
		}
	}
	
	public String toString() {
		switch (typeId) {
		case SHORTINT:
			return "SHORTINT";
		case INTEGER:
			return "INTEGER";
		case LONGINT:
			return "LONGINT";
		case REAL:
			return "REAL";
		case LONGREAL:
			return "LONGREAL";
		case CHAR:
			return "CHAR";
		case BOOLEAN:
			return "BOOLEAN";
		case STRING:
			return "STRING";
		case TYPE:
			return "TYPE";
		case VOID:
			return "VOID";
		case ARRAY:
			return "ARRAY " + this.arraySize() + " OF " + this.arrayType();
		case FUNCTION:
			String result = "FUNCTION(";
			for(int i = 0; i < this.funcParamCount(); i++) {
				if(this.funcIsVarParam(i))
					result += "VAR ";
				result += this.funcParamType(i);
				if(i != this.funcParamCount() - 1)
					result += ", ";
			}
			result += "): " + this.funcReturnType();
			return result;
		default:
			return "FHTAGN!";
		}
	}
}
