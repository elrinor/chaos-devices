package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;

import antlr.SemanticException;

public class TypedValue {
	Type type;
	Object value;
	
	public String toString() {
		if(value != null)
			return value.toString();
		else
			return "NULL";
	}

	
	public TypedValue(Type type, Object value) {
		this.type = type;
		this.value = value;
	}
	
	public TypedValue(TypedValue value) {
		TypedValue tmp = Type.cloneValue(value);
		this.type = tmp.type;
		this.value = tmp.value;
	}
	
	public TypedValue(Type type) {
		this.type = type;
		this.value = type.defaultValue();
	}
	
	protected TypedValue ConvertToNoCheck(Type type) throws SemanticException {
		if(this.type.type.isSimple() && this.type.type == type.type)
			return this;
		if(this.type.type.isNumeric()) 
		{
			if(this.type.type == BuiltInType.SHORTINT)
			{
				this.type = Type.IntegerType;
				this.value = (Integer)(int)(Short)this.value;
				return ConvertToNoCheck(type);
			}
			if(this.type.type == BuiltInType.INTEGER)
			{
				this.type = Type.LongIntType;
				this.value = (Long)(long)(Integer)this.value;
				return ConvertToNoCheck(type);
			}
			if(this.type.type == BuiltInType.LONGINT)
			{
				this.type = Type.RealType;
				this.value = (Float)(float)(Long)this.value;
				return ConvertToNoCheck(type);
			}
			if(this.type.type == BuiltInType.REAL)
			{
				this.type = Type.LongRealType;
				this.value = (Double)(double)(Float)this.value;
				return ConvertToNoCheck(type);
			}
			/*if(this.type.type == BuiltInType.LONGREAL)
			{
				this.type = Type.RealType;
				this.value = (Float)(float)(double)(Double)this.value;
				return ConvertToNoCheck(type);
			}*/
		}
		if(this.type.type == BuiltInType.STRING && type.type == BuiltInType.ARRAY)
		{
			if(((String)this.value).length() > type.arraySize())
				throw new RuntimeError(Error.TYPEMISMATCH, "Could not convert string of lenght " + ((String)this.value).length() + 
						" to array of size " + type.arraySize());
			String tmpString = (String)this.value;
			ArrayList<TypedValue> tmpArray = new ArrayList<TypedValue>();
			this.type = type.clone();
			for(int i = 0; i < tmpString.length(); i++)
				tmpArray.add(new TypedValue(Type.CharType, tmpString.charAt(i)));
			for(int i = tmpString.length(); i < this.type.arraySize(); i++)
				tmpArray.add(new TypedValue(Type.CharType, new Character('\0')));
			this.value = tmpArray;
			return this;
		}
		if(this.type.type == BuiltInType.ARRAY && type.type == BuiltInType.STRING)
		{
			String tmpString = new String();
			for(int i = 0; i < ((ArrayList<TypedValue>)this.value).size(); i++)
				tmpString += (Character)((ArrayList<TypedValue>)this.value).get(i).value;
			this.type = type;
			this.value = tmpString;
			return this;
		}
		if(this.type.type == BuiltInType.CHAR && type.type == BuiltInType.STRING)
		{
			this.type = type;
			this.value = new String() + (Character)this.value;
			return this;
		}
		if(this.type.type == BuiltInType.STRING && type.type == BuiltInType.CHAR)
		{
			if(((String)this.value).length() != 1)
				throw new RuntimeError(Error.TYPEMISMATCH, "Cannot convert from " + this.type + "[" + ((String)this.value).length() + "] to " + type);
			this.type = type;
			this.value = (Character)((String)this.value).charAt(0);
			return this;
		}
		this.type = type;
		return this;
	}
	
	public TypedValue ConvertTo(Type type) throws SemanticException {
		if(!this.type.convertableTo(type))
			throw new RuntimeError(Error.TYPEMISMATCH, "Cannot convert from " + this.type + " to " + type);
		return ConvertToNoCheck(type);
	}
	
	public static final TypedValue VoidTypedValue = new TypedValue(Type.VoidType, null);
}
