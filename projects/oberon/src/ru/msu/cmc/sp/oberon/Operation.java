package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;

import sun.util.BuddhistCalendar;

import antlr.SemanticException;

public enum Operation {
	
	PLUS{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			if (x.type.type == BuiltInType.SHORTINT)
			{
				Type.CheckShortBounds((double)(Short)x.value + (double)(Short)y.value);
				return new TypedValue(Type.ShortIntType, (Short)(short)((Short)x.value + (Short)y.value));
			}
			if (x.type.type == BuiltInType.INTEGER)
			{
				Type.CheckIntBounds((double)(Integer)x.value + (double)(Integer)y.value);
				return new TypedValue(Type.IntegerType, (Integer)(int)((Integer)x.value + (Integer)y.value));
			}
			if (x.type.type == BuiltInType.LONGINT)
			{
				Type.CheckLongBounds((double)(Long)x.value + (double)(Long)y.value);
				return new TypedValue(Type.LongIntType, (Long)(long)((Long)x.value + (Long)y.value));
			}
			if (x.type.type == BuiltInType.REAL)
				return new TypedValue(Type.RealType, (Float)(float)((Float)x.value + (Float)y.value));
			if (x.type.type == BuiltInType.LONGREAL)
				return new TypedValue(Type.LongRealType, (Double)(double)((Double)x.value + (Double)y.value));
			if (x.type.type == BuiltInType.CHAR)
				return new TypedValue(Type.StringType, new String() + (Character)x.value + (Character)y.value);
			if (x.type.type == BuiltInType.STRING)
				return new TypedValue(Type.StringType, (String)x.value + (String)y.value);
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) {
			return x;
		}
	},
	
	MINUS{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			return PLUS.DoPerform(x, DoPerform(y));
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			if (x.type.type == BuiltInType.SHORTINT)
				return new TypedValue(Type.ShortIntType, (Short)((short)(-(Short)x.value)));
			if (x.type.type == BuiltInType.INTEGER)
				return new TypedValue(Type.IntegerType, (Integer)((int)(-(Integer)x.value)));
			if (x.type.type == BuiltInType.LONGINT)
				return new TypedValue(Type.LongIntType, (Long)((long)(-(Long)x.value)));
			if (x.type.type == BuiltInType.REAL)
				return new TypedValue(Type.RealType, (Float)((float)(-(Float)x.value)));
			if (x.type.type == BuiltInType.LONGREAL)
				return new TypedValue(Type.LongRealType, (Double)((double)(-(Double)x.value)));
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	TIMES{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			if (x.type.type == BuiltInType.SHORTINT)
			{
				Type.CheckShortBounds((double)(Short)x.value * (double)(Short)y.value);
				return new TypedValue(Type.ShortIntType, (Short)(short)((Short)x.value * (Short)y.value));
			}
			if (x.type.type == BuiltInType.INTEGER)
			{
				Type.CheckIntBounds((double)(Integer)x.value * (double)(Integer)y.value);
				return new TypedValue(Type.IntegerType, (Integer)(int)((Integer)x.value * (Integer)y.value));
			}
			if (x.type.type == BuiltInType.LONGINT)
			{
				Type.CheckLongBounds((double)(Long)x.value * (double)(Long)y.value);
				return new TypedValue(Type.LongIntType, (Long)(long)((Long)x.value * (Long)y.value));
			}
			if (x.type.type == BuiltInType.REAL)
				return new TypedValue(Type.RealType, (Float)(float)((Float)x.value * (Float)y.value));
			if (x.type.type == BuiltInType.LONGREAL)
				return new TypedValue(Type.LongRealType, (Double)(double)((Double)x.value * (Double)y.value));
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	DIVIDE{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			if (x.type.type == BuiltInType.SHORTINT)
			{
				if((Short)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.RealType, (Float)(float)((Float)(float)(Short)x.value / (Float)(float)(Short)y.value));
			}
			if (x.type.type == BuiltInType.INTEGER)
			{
				if((Integer)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.RealType, (Float)(float)((Float)(float)(Integer)x.value / (Float)(float)(Integer)y.value));
			}
			if (x.type.type == BuiltInType.LONGINT)
			{
				if((Long)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.RealType, (Float)(float)((Float)(float)(Long)x.value / (Float)(float)(Long)y.value));
			}
			if (x.type.type == BuiltInType.REAL)
			{
				if((Float)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.RealType, (Float)(float)((Float)x.value / (Float)y.value));
			}
			if (x.type.type == BuiltInType.LONGREAL)
			{
				if((Double)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.LongRealType, (Double)(double)((Double)x.value / (Double)y.value));
			}
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	DIV{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			if (x.type.type == BuiltInType.SHORTINT)
			{
				if((Short)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.ShortIntType, (Short)(short)(int)((Short)x.value / (Short)y.value));
			}
			if (x.type.type == BuiltInType.INTEGER)
			{
				if((Integer)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.IntegerType, (Integer)(int)((Integer)x.value / (Integer)y.value));
			}
			if (x.type.type == BuiltInType.LONGINT)
			{
				if((Long)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.LongIntType, (Long)(long)((Long)x.value / (Long)y.value));
			}
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	MOD{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			if (x.type.type == BuiltInType.SHORTINT)
			{
				if((Short)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.ShortIntType, (Short)(short)((Short)x.value % (Short)y.value));
			}
			if (x.type.type == BuiltInType.INTEGER)
			{
				if((Integer)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.IntegerType, (Integer)(int)((Integer)x.value % (Integer)y.value));
			}
			if (x.type.type == BuiltInType.LONGINT)
			{
				if((Long)y.value == 0) throw new RuntimeError(Error.DIVISIONBYZERO);
				return new TypedValue(Type.LongIntType, (Long)(long)((Long)x.value % (Long)y.value));
			}
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	AND{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			if (x.type.type == BuiltInType.BOOLEAN)
				return new TypedValue(Type.BooleanType, (Boolean)x.value && (Boolean)y.value);
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},

	OR{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			if (x.type.type == BuiltInType.BOOLEAN)
				return new TypedValue(Type.BooleanType, (Boolean)x.value || (Boolean)y.value);
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	NOT{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			if (x.type.type == BuiltInType.BOOLEAN)
				return new TypedValue(Type.BooleanType, !(Boolean)x.value);
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	EQUAL{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			if (x.type.type == BuiltInType.SHORTINT)
				return new TypedValue(Type.BooleanType, (short)(Short)x.value     == (short)(Short)y.value);
			if (x.type.type == BuiltInType.INTEGER) 
				return new TypedValue(Type.BooleanType, (int)(Integer)x.value     == (int)(Integer)y.value);
			if (x.type.type == BuiltInType.LONGINT)
				return new TypedValue(Type.BooleanType, (long)(Long)x.value       == (long)(Long)y.value);
			if (x.type.type == BuiltInType.REAL)
				return new TypedValue(Type.BooleanType, (float)(Float)x.value     == (float)(Float)y.value);
			if (x.type.type == BuiltInType.LONGREAL)
				return new TypedValue(Type.BooleanType, (double)(Double)x.value   == (double)(Double)y.value);
			if (x.type.type == BuiltInType.CHAR)
				return new TypedValue(Type.BooleanType, (char)(Character)x.value  == (char)(Character)y.value);
			if (x.type.type == BuiltInType.STRING)
				return new TypedValue(Type.BooleanType, ((String)x.value).equals((String)y.value));
			if (x.type.type == BuiltInType.BOOLEAN)
				return new TypedValue(Type.BooleanType, (boolean)(Boolean)x.value == (boolean)(Boolean)y.value);
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},

	NOTEQUAL{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			return NOT.DoPerform(EQUAL.DoPerform(x, y));
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	LESS{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			if (x.type.type == BuiltInType.SHORTINT)
				return new TypedValue(Type.BooleanType, (Short)x.value     < (Short)y.value);
			if (x.type.type == BuiltInType.INTEGER)
				return new TypedValue(Type.BooleanType, (Integer)x.value   < (Integer)y.value);
			if (x.type.type == BuiltInType.LONGINT)
				return new TypedValue(Type.BooleanType, (Long)x.value      < (Long)y.value);
			if (x.type.type == BuiltInType.REAL)
				return new TypedValue(Type.BooleanType, (Float)x.value     < (Float)y.value);
			if (x.type.type == BuiltInType.LONGREAL)
				return new TypedValue(Type.BooleanType, (Double)x.value    < (Double)y.value);
			if (x.type.type == BuiltInType.CHAR)
				return new TypedValue(Type.BooleanType, (Character)x.value < (Character)y.value);
			if (x.type.type == BuiltInType.STRING)
				return new TypedValue(Type.BooleanType, ((String)x.value).compareTo((String)y.value) < 0);
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	LESSOREQUAL{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			return OR.DoPerform(LESS.DoPerform(x, y), EQUAL.DoPerform(x, y));
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},

	GREATER{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			return NOT.DoPerform(LESSOREQUAL.DoPerform(x, y));
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	GREATEROREQUAL{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			return NOT.DoPerform(LESS.DoPerform(x, y));
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
	},
	
	ASSIGN{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			     if (x.type.type == BuiltInType.SHORTINT)
				x.value = (Short)y.value;
			else if (x.type.type == BuiltInType.INTEGER)
				x.value = (Integer)y.value;
			else if (x.type.type == BuiltInType.LONGINT)
				x.value = (Long)y.value;
			else if (x.type.type == BuiltInType.REAL)
				x.value = (Float)y.value;
			else if (x.type.type == BuiltInType.LONGREAL)
				x.value = (Double)y.value;
			else if (x.type.type == BuiltInType.CHAR)
				x.value = (Character)y.value;
			else if (x.type.type == BuiltInType.STRING)
				x.value = (String)y.value;
			else if (x.type.type == BuiltInType.BOOLEAN)
				x.value = (Boolean)y.value;
			else if (x.type.type ==  BuiltInType.ARRAY) // Arrays can be assigned only if they have the same type
				x.value = Type.cloneValue(y).value;
			else throw new RuntimeError(Error.TYPEMISMATCH);
			return new TypedValue(Type.VoidType, null);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue Perform(TypedValue x, TypedValue y) throws SemanticException, RuntimeError {
			return DoPerform(x, Type.cloneValue(y).ConvertTo(x.type));
			
		}
	},
	
	INDEX{
		public TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		}
		public TypedValue DoPerform(TypedValue x) throws RuntimeError {
			throw new RuntimeError(Error.TYPEMISMATCH);
		};
		public TypedValue Perform(TypedValue x, TypedValue y) throws SemanticException, RuntimeError {
			int index = (int)(long)(Long)Type.cloneValue(y).ConvertTo(Type.LongIntType).value;
			if(x.type.type != BuiltInType.ARRAY && x.type.type != BuiltInType.STRING)
				throw new RuntimeError(Error.TYPEMISMATCH);
			if(x.type.type == BuiltInType.STRING)
			{
				if(index < 0 || index >= ((String)x.value).length())
					throw new RuntimeError(Error.OUTOFBOUNDS);
				return new TypedValue(Type.CharType, (Character)((String)x.value).charAt(index));
			}
			else
			{
				if(index < 0 || (index >= x.type.arraySize() && x.type.arraySize() != 0) || index >= ((ArrayList<TypedValue>)x.value).size())
					throw new RuntimeError(Error.OUTOFBOUNDS);
				return ((ArrayList<TypedValue>)x.value).get(index);
			}
		}
	};

	protected abstract TypedValue DoPerform(TypedValue x, TypedValue y) throws RuntimeError;
	protected abstract TypedValue DoPerform(TypedValue x) throws RuntimeError;
	public TypedValue Perform(TypedValue x, TypedValue y) throws SemanticException, RuntimeError {
		x = Type.cloneValue(x);
		y = Type.cloneValue(y);
		Type type = Type.overType(x.type, y.type);
		return DoPerform(x.ConvertTo(type), y.ConvertTo(type));
	}
	public TypedValue Perform(TypedValue x) throws RuntimeError {
		x = Type.cloneValue(x);
		return DoPerform(x);
	}
	public Type returnType(Type x, Type y) throws SemanticException, RuntimeError {
		TypedValue vx = x.defaultValue();
		TypedValue vy = y.defaultValue();
		if(vy.type.type.isNumeric() && !this.equals(INDEX)) {
			if (vy.type.type == BuiltInType.SHORTINT)
				vy.value = (Short)(short)1;
			else if (vy.type.type == BuiltInType.INTEGER)
				vy.value = (Integer)1;
			else if (vy.type.type == BuiltInType.LONGINT)
				vy.value = (Long)1l;
			else if (vy.type.type == BuiltInType.REAL)
				vy.value = (Float)1.0f;
			else if (vy.type.type == BuiltInType.LONGREAL)
				vy.value = (Double)1.0;
		}
		if(vy.type.type == BuiltInType.STRING && vx.type.type == BuiltInType.CHAR)
			vy.value = "a";
		if(this.equals(INDEX)) {
			if(!x.type.equals(BuiltInType.ARRAY))
				throw new RuntimeError(Error.TYPEMISMATCH);
			if(!y.convertableTo(Type.LongIntType))
				throw new RuntimeError(Error.TYPEMISMATCH);
			return x.arrayOf();
		}
		return Perform(vx, vy).type;
	}
	public Type returnType(Type x) throws SemanticException {
		TypedValue vx = x.defaultValue();
		TypedValue vr = this.Perform(vx);
		return vr.type;
	}
}
