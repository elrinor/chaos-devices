package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;

import antlr.SemanticException;

public class Type {
	public BuiltInType type;
	public String name;
	protected Object f1, f2, f3, f4;
	
	public Type clone() {
		Type type = new Type(this.type, name);
		if(this.type == BuiltInType.ARRAY)
		{
			type.f1 = this.arrayOf().clone();
			type.f2 = this.arraySize();
		}
		// We don't need to clone procs
		return type;
	}

	public String toString() {
		StringBuffer result = new StringBuffer("");
		result.append(type.toString());
		if(type == BuiltInType.ARRAY)
			result.append(" " + arraySize() + " OF " + arrayOf().toString());
		else if(type == BuiltInType.PROCEDURE) {
			result.append(" " + returnType().toString() + " (");
			for(int i = 0; i < paramTypes().size(); i++) {
				if(isRefParam().get(i))
					result.append("VAR ");
				result.append(paramTypes().get(i).toString());
				if(i + 1 < paramTypes().size())
					result.append(", ");
			}
			result.append(")");
		}
		return result.toString();
	}
	
	public TypedValue defaultValue() {
		switch (type) {
		case VOID:
			return new TypedValue(Type.VoidType, null);
		case STRING:
			return new TypedValue(Type.StringType, "");
		case BOOLEAN:
			return new TypedValue(Type.BooleanType, (Boolean)false);
		case CHAR:
			return new TypedValue(Type.CharType, (Character)'\0');
		case SHORTINT:
			return new TypedValue(Type.ShortIntType, (Short)(short)0);
		case INTEGER:
			return new TypedValue(Type.IntegerType, (Integer)0);
		case LONGINT:
			return new TypedValue(Type.LongIntType, (Long)0l);
		case REAL:
			return new TypedValue(Type.RealType, (Float)0.0f);
		case LONGREAL:
			return new TypedValue(Type.LongRealType, (Double)0.0);
		case ARRAY:
			ArrayList<TypedValue> l = new ArrayList<TypedValue>();
			for(int i = 0; i < arraySize(); i++)
				l.add(arrayOf().defaultValue());
			return new TypedValue(this.clone(), l);
		case PROCEDURE:
			return null;
		}
		return null;
	}
	
	public static TypedValue cloneValue(TypedValue v) {
		switch (v.type.type) {
		case VOID:
			return null;
		case STRING:
			return new TypedValue(v.type.clone(), new String((String)v.value));
		case BOOLEAN:
			return new TypedValue(v.type.clone(), new Boolean((Boolean)v.value));
		case CHAR:
			return new TypedValue(v.type.clone(), new Character((Character)v.value));
		case SHORTINT:
			return new TypedValue(v.type.clone(), new Short((Short)v.value));
		case INTEGER:
			return new TypedValue(v.type.clone(), new Integer((Integer)v.value));
		case LONGINT:
			return new TypedValue(v.type.clone(), new Long((Long)v.value));
		case REAL:
			return new TypedValue(v.type.clone(), new Float((Float)v.value));
		case LONGREAL:
			return new TypedValue(v.type.clone(), new Double((Double)v.value));
		case ARRAY:
			ArrayList<Object> l = new ArrayList<Object>();
			for(int i = 0; i < ((ArrayList<Object>)v.value).size(); i++)
				l.add(Type.cloneValue((TypedValue)((ArrayList<Object>)v.value).get(i)));
			return new TypedValue(v.type.clone(), l);
		case PROCEDURE:
			return null;
		}
		return null;
	}
	
	protected static int unnamedNumber = 0;
	protected static String newUnnamed() {
		return "Unnamed" + unnamedNumber++;
	}
	
	// Create as Simple Type
	public Type(BuiltInType type, String name) {
		this.type = type;
		this.name = name;
	}
	public Type(BuiltInType type) {
		this(type, newUnnamed());
	}
	
	// Create as Array
	public Type(BuiltInType type, String name, Type arrayOf, int arraySize){
		this(type, name);
		/*if(type != BuiltInType.ARRAY)
			throw new SemanticException("Only Array type is allowed in this constructor!");*/
		f1 = arrayOf;
		f2 = (Integer)arraySize;
	}
	public Type(BuiltInType type, Type arrayOf, int arraySize){
		this(type, newUnnamed(), arrayOf, arraySize);
	}
	
	// Create as Proc
	public Type(BuiltInType type, String name, ArrayList<Type> paramTypes, ArrayList<Boolean> isRefParam, Type returnType, boolean isInvoke){
		this(type, name);
		f1 = paramTypes;
		f2 = isRefParam;
		f3 = returnType;
		f4 = isInvoke;
	}

	public Type(BuiltInType type, String name, ArrayList<Type> paramTypes, ArrayList<Boolean> isRefParam, Type returnType){
		this(type, name, paramTypes, isRefParam, returnType, false);
	}
	public Type(BuiltInType type, String name, ArrayList<Type> paramTypes, ArrayList<Boolean> isRefParam){
		this(type, name, paramTypes, isRefParam, Type.VoidType);
	}
	public Type(BuiltInType type, ArrayList<Type> paramTypes, ArrayList<Boolean> isRefParam, Type returnType){
		this(type, newUnnamed(), paramTypes, isRefParam, returnType);
	}
	public Type(BuiltInType type, ArrayList<Type> paramTypes, ArrayList<Boolean> isRefParam){
		this(type, newUnnamed(), paramTypes, isRefParam, Type.VoidType);
	}
	
	// Array & Proc Helpers
	protected Type arrayOf() {
		return (Type) f1;
	}
	protected int arraySize() {
		return ((Integer) f2);
	}
	protected ArrayList<Type> paramTypes() {
		return (ArrayList<Type>) f1;
	}
	protected ArrayList<Boolean> isRefParam() {
		return (ArrayList<Boolean>) f2;
	}
	protected Type returnType() {
		return (Type) f3;
	}
	protected boolean isInvoke() {
		return (Boolean) f4;
	}
	
	public boolean equals(Object param) {
		Type type = (Type)param;
		if(this.type != type.type)
			return false;
		else if(this.type == BuiltInType.ARRAY && !(this.arrayOf().equals(type.arrayOf()) && 
				(this.arraySize() == type.arraySize() || this.arraySize() == 0 || type.arraySize() == 0)))
			return false;
		else if(this.type == BuiltInType.PROCEDURE)
		{
			if(!this.returnType().equals(type.returnType()))
				return false;
			for(int i = 0; i < this.paramTypes().size(); i++)
				if(!(this.paramTypes().get(i).equals(type.paramTypes().get(i)) && this.isRefParam().get(i) == type.isRefParam().get(i)))
					return false;
			return true;
		}
		else
			return true;
	}
	
	public boolean convertableTo(Type type) {
		if(equals(type))
			return true;
		if(this.type.isNumeric() && type.type.isNumeric() && this.type.typeWidth() <= type.type.typeWidth())
			return true;
		/*if(this.type == BuiltInType.LONGREAL && type.type == BuiltInType.REAL)
			return true;*/
		if(this.type == BuiltInType.CHAR && type.type == BuiltInType.STRING)
			return true;
		if(this.type == BuiltInType.STRING && type.type == BuiltInType.ARRAY && type.arrayOf().type == BuiltInType.CHAR)
			return true;
		if(type.type == BuiltInType.STRING && this.type == BuiltInType.ARRAY && this.arrayOf().type == BuiltInType.CHAR)
			return true;
		if(this.type == BuiltInType.STRING && type.type == BuiltInType.CHAR)
			return true;
		return false;
	}
	
	public static Type overType(Type type1, Type type2) {
		if(type1.convertableTo(type2) || type2.convertableTo(type1))
		{
			if(type1.type == BuiltInType.STRING || type2.type == BuiltInType.STRING)
				return Type.StringType;
			if(type1.type == BuiltInType.ARRAY && type1.arrayOf().type == BuiltInType.CHAR)
				return Type.StringType;
			if(type1.type == BuiltInType.ARRAY && type1.arraySize() == 0)
				return type1;
			if(type2.type == BuiltInType.ARRAY && type2.arraySize() == 0)
				return type1;
			if(type1.convertableTo(type2))
				return type2;
			else
				return type1;
			
		}
		else
			return Type.VoidType;
	}
	
	public static void CheckShortBounds(double value) throws RuntimeError {
		if(value < Short.MIN_VALUE || value > Short.MAX_VALUE)
			throw new RuntimeError(Error.INTEGEROVERFLOW);
	}

	public static void CheckIntBounds(double value) throws RuntimeError {
		if(value < Integer.MIN_VALUE || value > Integer.MAX_VALUE)
			throw new RuntimeError(Error.INTEGEROVERFLOW);
	}
	
	public static void CheckLongBounds(double value) throws RuntimeError {
		if(value < Long.MIN_VALUE || value > Long.MAX_VALUE)
			throw new RuntimeError(Error.INTEGEROVERFLOW);
	}
	
	// Useful consts & methods
	public static final Type VoidType = new Type(BuiltInType.VOID);
	public static final Type ShortIntType = new Type(BuiltInType.SHORTINT);
	public static final Type IntegerType = new Type(BuiltInType.INTEGER);
	public static final Type LongIntType = new Type(BuiltInType.LONGINT);
	public static final Type RealType = new Type(BuiltInType.REAL);
	public static final Type LongRealType = new Type(BuiltInType.LONGREAL);
	public static final Type BooleanType = new Type(BuiltInType.BOOLEAN);
	public static final Type CharType = new Type(BuiltInType.CHAR);
	public static final Type StringType = new Type(BuiltInType.STRING);
	public static final Type SimpleProcType = new Type(BuiltInType.PROCEDURE, new ArrayList<Type>(), new ArrayList<Boolean>());
	
	static Type CreateInvokeType(int paramCount, Type paramType, boolean isVar0) {
		Type type = new Type(BuiltInType.PROCEDURE, newUnnamed(), new ArrayList<Type>(), new ArrayList<Boolean>(), Type.IntegerType, true);
		for(int i = 0; i < paramCount; i++)
		{
			type.paramTypes().add(paramType);
			if(i == 0)
				type.isRefParam().add(isVar0);
			else
				type.isRefParam().add(false);
		}
		return type;
	}

	static Type CreateInvokeType(int paramCount, Type param0Type, Type param1Type) {
		Type type = new Type(BuiltInType.PROCEDURE, newUnnamed(), new ArrayList<Type>(), new ArrayList<Boolean>(), Type.IntegerType, true);
		for(int i = 0; i < paramCount; i++)
		{
			if(i == 0)
				type.paramTypes().add(param0Type);
			else if (i == 1)
				type.paramTypes().add(param1Type);
			else
				type.paramTypes().add(param1Type);
			type.isRefParam().add(false);
		}
		return type;
	}
}








