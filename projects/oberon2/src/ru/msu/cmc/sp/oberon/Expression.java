package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;

public abstract class Expression {
	// Useful consts
	public static final int VOID =         0;
	public static final int PLUS =         1;
	public static final int MINUS =        2;
	public static final int DIVIDE =       3;
	public static final int TIMES =        4;
	public static final int DIV =          5;
	public static final int MOD =          6;
	public static final int GREATER =      7;
	public static final int LESS =         8;
	public static final int EQUAL =        9;
	public static final int GREATEREQUAL = 10;
	public static final int LESSEQUAL =    11;
	public static final int NOTEQUAL =     12;
	public static final int NOT =          13;
	public static final int OR =           14;
	public static final int AND =          15;
	
	// Evaluation support functions
	void mustBeShort(double x) throws Error {
		if(x < Short.MIN_VALUE || x > Short.MAX_VALUE)
			throw new Error(Error.INTOVERFLOW);
	}
	void mustBeInt(double x) throws Error {
		if(x < Integer.MIN_VALUE || x > Integer.MAX_VALUE)
			throw new Error(Error.INTOVERFLOW);
	}
	void mustBeLong(double x) throws Error {
		if(x < Long.MIN_VALUE || x > Long.MAX_VALUE)
			throw new Error(Error.INTOVERFLOW);
	}
	
	// Constant
	public static class Constant extends Expression{
		private Type type;
		private Object value;
		public Object evaluate() {
			return value;
		}
		public Type returnType() {
			return type;
		}
		public Constant(Type type, Object value) {
			this.type = type;
			this.value = value;
		}
	}
	public static Expression Constant(Type type, Object value) {
		return new Constant(type, value);
	}
	
	// Variable
	public static class Variable extends Expression{
		private int varNo;
		private Expression index;
		public int getVarNo() {
			return varNo;
		}
		public Expression getIndex() {
			return index;
		}
		public Object evaluate() throws Error {
			if(index == null)
				return Module.idents.get(varNo).getValue();
			else
				return ((ArrayList<Object>)Module.idents.get(varNo).getValue()).get((int)(long)(Long)index.evaluate());
		}
		public Type returnType() {
			if(index == null)
				return Module.idents.get(varNo).getType();
			else
				return Module.idents.get(varNo).getType().arrayType();
		}
		public Variable(int varNo, Expression index) {
			this.varNo = varNo;
			this.index = index;
		}
	}
	private static Expression DoVariable(int varNo, Expression index) throws Error {
		if(index != null && Module.idents.get(varNo).getType().typeId() != Type.ARRAY)
			throw new Error(Error.COMPILEERROR, "Cannot index non-array variable");
		if(index != null)
			index = TypeCast(Type.LongInt, index);
		if(Module.idents.get(varNo).isConst())
			return Constant(Module.idents.get(varNo).getType(), Module.idents.get(varNo).getValue());
		return new Variable(varNo, index);
	}
	public static Expression Variable(int varNo, Expression index) throws Error {
		return DoVariable(varNo, index);
	}
	
	// Type casts
	public static class ShortToInt extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			return (Integer)(int)(short)(Short)e.evaluate();
		}
		public ShortToInt(Expression e) {
			this.e = e;
		}
	}
	public static class IntToLong extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			return (Long)(long)(int)(Integer)e.evaluate();
		}
		public IntToLong(Expression e) {
			this.e = e;
		}
	}
	public static class LongToReal extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			return (Float)(float)(long)(Long)e.evaluate();
		}
		public LongToReal(Expression e) {
			this.e = e;
		}
	}
	public static class RealToDouble extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.LongReal;
		}
		public Object evaluate() throws Error {
			return (Double)(double)(float)(Float)e.evaluate();
		}
		public RealToDouble(Expression e) {
			this.e = e;
		}
	}
	public static class StringToArray extends Expression{
		private Expression e;
		private Type returnType;
		public Type returnType() {
			return returnType;
		}
		public Object evaluate() throws Error {
			String s = (String)e.evaluate();
			if(returnType.arraySize() != 0 && returnType.arraySize() < s.length())
				throw new Error(Error.DOMAINERROR);
			ArrayList<Character> result = new ArrayList<Character>();
			for(int i = 0; i < s.length(); i++)
				result.add(s.charAt(i));
			for(int i = s.length(); i < returnType.arraySize(); i++)
				result.add('\0');
			return result;
		}
		public StringToArray(Expression e, int arraySize) throws Error {
			this.e = e;
			returnType = Type.Array(arraySize, Type.Char);
		}
	}
	public static class ArrayToString extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.String;
		}
		public Object evaluate() throws Error {
			ArrayList<Character> t = (ArrayList<Character>)e.evaluate();
			String s = "";
			for(int i = 0; i < t.size(); i++)
				s += t.get(i);
			return s;
		}
		public ArrayToString(Expression e) throws Error {
			this.e = e;
		}
	}
	public static class CharToString  extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.String;
		}
		public Object evaluate() throws Error {
			return ((Character)e.evaluate()).toString();
		}
		public CharToString(Expression e) throws Error {
			this.e = e;
		}
	}
	private static Expression DoTypeCast(Type to, Expression e) throws Error {
		if(e.returnType().equals(to))
			return e;
		if(!e.returnType().canBeConvertedTo(to))
			throw new Error(Error.COMPILEERROR, "Cannot convert " + e.returnType().toString() + " to " + to.toString());
		if(e.returnType().isNumeric()) {
			if(e.returnType().typeId() == Type.SHORTINT)
				return TypeCast(to, new ShortToInt(e));
			else if(e.returnType().typeId() == Type.INTEGER)
				return TypeCast(to, new IntToLong(e));
			else if(e.returnType().typeId() == Type.LONGINT)
				return TypeCast(to, new LongToReal(e));
			else /* if(from.typeId() == Type.REAL) // we don't need to check this */
				return TypeCast(to, new RealToDouble(e));
				
		}
		else if(e.returnType().typeId() == Type.STRING && to.typeId() == Type.ARRAY && to.arrayType().typeId() == Type.CHAR) {
			return new StringToArray(e, to.arraySize());
		}
		else if(e.returnType().typeId() == Type.ARRAY && e.returnType().arrayType().typeId() == Type.CHAR && to.typeId() == Type.STRING) {
			return new ArrayToString(e);
		}
		else if(e.returnType().typeId() == Type.CHAR && to.typeId() == Type.STRING) {
			return new CharToString(e);
		}
		else
			throw new Error(Error.COMPILEERROR, "Cannot convert " + e.returnType().toString() + " to " + to.toString());
	}
	public static Expression TypeCast(Type to, Expression e) throws Error {
		Expression result = DoTypeCast(to, e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	
	// Operations
	public static class ShortPlus extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.ShortInt;
		}
		public Object evaluate() throws Error {
			short v1 = (Short)e1.evaluate();
			short v2 = (Short)e2.evaluate();
			mustBeShort((double)v1 + (double)v2);
			return (Short)(short)((Short)v1 + (Short)v2);
		}
		public ShortPlus(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class IntPlus extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			int v1 = (Integer)e1.evaluate();
			int v2 = (Integer)e2.evaluate();
			mustBeInt((double)v1 + (double)v2);
			return (Integer)(int)((Integer)v1 + (Integer)v2);
		}
		public IntPlus(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class LongPlus extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			long v1 = (Long)e1.evaluate();
			long v2 = (Long)e2.evaluate();
			mustBeLong((double)v1 + (double)v2);
			return (Long)(long)((Long)v1 + (Long)v2);
		}
		public LongPlus(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class RealPlus extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			return (Float)(float)((Float)e1.evaluate() + (Float)e2.evaluate());
		}
		public RealPlus(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class DoublePlus extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.LongReal;
		}
		public Object evaluate() throws Error {
			return (Double)(double)((Double)e1.evaluate() + (Double)e2.evaluate());
		}
		public DoublePlus(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class ShortUMinus extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.ShortInt;
		}
		public Object evaluate() throws Error {
			return (Short)(short)(-(Short)e.evaluate());
		}
		public ShortUMinus(Expression e) {
			this.e = e;
		}
	}
	public static class IntUMinus extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			return (Integer)(int)(-(Integer)e.evaluate());
		}
		public IntUMinus(Expression e) {
			this.e = e;
		}
	}
	public static class LongUMinus extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			return (Long)(long)(-(Long)e.evaluate());
		}
		public LongUMinus(Expression e) {
			this.e = e;
		}
	}		
	public static class RealUMinus extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			return (Float)(float)(-(Float)e.evaluate());
		}
		public RealUMinus(Expression e) {
			this.e = e;
		}
	}		
	public static class DoubleUMinus extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.LongReal;
		}
		public Object evaluate() throws Error {
			return (Double)(double)(-(Double)e.evaluate());
		}
		public DoubleUMinus(Expression e) {
			this.e = e;
		}
	}		
	public static class ShortTimes extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.ShortInt;
		}
		public Object evaluate() throws Error {
			short v1 = (Short)e1.evaluate();
			short v2 = (Short)e2.evaluate();
			mustBeShort((double)v1 * (double)v2);
			return (Short)(short)((Short)v1 * (Short)v2);
		}
		public ShortTimes(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class IntTimes extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			int v1 = (Integer)e1.evaluate();
			int v2 = (Integer)e2.evaluate();
			mustBeInt((double)v1 * (double)v2);
			return (Integer)(int)((Integer)v1 * (Integer)v2);
		}
		public IntTimes(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class LongTimes extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			long v1 = (Long)e1.evaluate();
			long v2 = (Long)e2.evaluate();
			mustBeLong((double)v1 * (double)v2);
			return (Long)(long)((Long)v1 * (Long)v2);
		}
		public LongTimes(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class RealTimes extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			return (Float)(float)((Float)e1.evaluate() * (Float)e2.evaluate());
		}
		public RealTimes(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class DoubleTimes extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.LongReal;
		}
		public Object evaluate() throws Error {
			return (Double)(double)((Double)e1.evaluate() * (Double)e2.evaluate());
		}
		public DoubleTimes(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class ShortDivide extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			float v2 = (Short)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Float)(float)((Short)e1.evaluate() / v2);
		}
		public ShortDivide(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class IntDivide extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			float v2 = (Integer)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Float)(float)((Integer)e1.evaluate() / v2);
		}
		public IntDivide(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class LongDivide extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			float v2 = (Long)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Float)(float)((Long)e1.evaluate() / v2);
		}
		public LongDivide(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class RealDivide extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			float v2 = (Float)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Float)(float)((Float)e1.evaluate() / v2);
		}
		public RealDivide(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class DoubleDivide extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.LongReal;
		}
		public Object evaluate() throws Error {
			double v2 = (Double)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Double)(double)((Double)e1.evaluate() / v2);
		}
		public DoubleDivide(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class ShortDiv extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.ShortInt;
		}
		public Object evaluate() throws Error {
			short v2 = (Short)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Short)(short)((Short)e1.evaluate() / v2);
		}
		public ShortDiv(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class IntDiv extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			int v2 = (Integer)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Integer)(int)((Integer)e1.evaluate() / v2);
		}
		public IntDiv(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class LongDiv extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			long v2 = (Long)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Long)(long)((Long)e1.evaluate() / v2);
		}
		public LongDiv(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class ShortMod extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.ShortInt;
		}
		public Object evaluate() throws Error {
			short v2 = (Short)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Short)(short)((Short)e1.evaluate() % v2);
		}
		public ShortMod(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class IntMod extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			int v2 = (Integer)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Integer)(int)((Integer)e1.evaluate() % v2);
		}
		public IntMod(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class LongMod extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			long v2 = (Long)e2.evaluate();
			if(v2 == 0)
				throw new Error(Error.ZERODIVIDE);
			return (Long)(long)((Long)e1.evaluate() % v2);
		}
		public LongMod(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static abstract class Compare extends Expression{
		protected Expression e1, e2;
		public Type returnType() {
			return Type.Boolean;
		}
		public Compare(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class ShortEqual extends Compare{
		public Object evaluate() throws Error {
			return (short)(Short)e1.evaluate() == (short)(Short)e2.evaluate();
		}
		public ShortEqual(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class IntEqual extends Compare{
		public Object evaluate() throws Error {
			return (int)(Integer)e1.evaluate() == (int)(Integer)e2.evaluate();
		}
		public IntEqual(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class LongEqual extends Compare{
		public Object evaluate() throws Error {
			return (long)(Long)e1.evaluate() == (long)(Long)e2.evaluate();
		}
		public LongEqual(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class RealEqual extends Compare{
		public Object evaluate() throws Error {
			return (float)(Float)e1.evaluate() == (float)(Float)e2.evaluate();
		}
		public RealEqual(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class DoubleEqual extends Compare{
		public Object evaluate() throws Error {
			return (double)(Double)e1.evaluate() == (double)(Double)e2.evaluate();
		}
		public DoubleEqual(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class BooleanEqual extends Compare{
		public Object evaluate() throws Error {
			return (boolean)(Boolean)e1.evaluate() == (boolean)(Boolean)e2.evaluate();
		}
		public BooleanEqual(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class CharEqual extends Compare{
		public Object evaluate() throws Error {
			return (char)(Character)e1.evaluate() == (char)(Character)e2.evaluate();
		}
		public CharEqual(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class StringEqual extends Compare{
		public Object evaluate() throws Error {
			return ((String)e1.evaluate()).equals((String)e2.evaluate());
		}
		public StringEqual(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class ShortGreater extends Compare{
		public Object evaluate() throws Error {
			return (Short)e1.evaluate() > (Short)e2.evaluate();
		}
		public ShortGreater(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class IntGreater extends Compare{
		public Object evaluate() throws Error {
			return (Integer)e1.evaluate() > (Integer)e2.evaluate();
		}
		public IntGreater(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class LongGreater extends Compare{
		public Object evaluate() throws Error {
			return (Long)e1.evaluate() > (Long)e2.evaluate();
		}
		public LongGreater(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class RealGreater extends Compare{
		public Object evaluate() throws Error {
			return (Float)e1.evaluate() > (Float)e2.evaluate();
		}
		public RealGreater(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class DoubleGreater extends Compare{
		public Object evaluate() throws Error {
			return (Double)e1.evaluate() > (Double)e2.evaluate();
		}
		public DoubleGreater(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class ShortLess extends Compare{
		public Object evaluate() throws Error {
			return (Short)e1.evaluate() < (Short)e2.evaluate();
		}
		public ShortLess(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class IntLess extends Compare{
		public Object evaluate() throws Error {
			return (Integer)e1.evaluate() < (Integer)e2.evaluate();
		}
		public IntLess(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class LongLess extends Compare{
		public Object evaluate() throws Error {
			return (Long)e1.evaluate() < (Long)e2.evaluate();
		}
		public LongLess(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class RealLess extends Compare{
		public Object evaluate() throws Error {
			return (Float)e1.evaluate() < (Float)e2.evaluate();
		}
		public RealLess(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class DoubleLess extends Compare{
		public Object evaluate() throws Error {
			return (Double)e1.evaluate() < (Double)e2.evaluate();
		}
		public DoubleLess(Expression e1, Expression e2) {
			super(e1, e2);
		}
	}
	public static class And extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Boolean;
		}
		public Object evaluate() throws Error {
			return (Boolean)e1.evaluate() && (Boolean)e2.evaluate();
		}
		public And(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class Or extends Expression{
		private Expression e1, e2;
		public Type returnType() {
			return Type.Boolean;
		}
		public Object evaluate() throws Error {
			return (Boolean)e1.evaluate() || (Boolean)e2.evaluate();
		}
		public Or(Expression e1, Expression e2) {
			this.e1 = e1; this.e2 = e2;
		}
	}
	public static class Not extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Boolean;
		}
		public Object evaluate() throws Error {
			return !(Boolean)e.evaluate();
		}
		public Not(Expression e) {
			this.e = e;
		}
	}
	private static Expression DoOperation(int opCode, Expression e) throws Error {
		if(opCode == MINUS && e.returnType().isNumeric()) {
			if(e.returnType().typeId() == Type.SHORTINT)
				return new ShortUMinus(e);
			else if(e.returnType().typeId() == Type.INTEGER)
				return new IntUMinus(e);
			else if(e.returnType().typeId() == Type.LONGINT)
				return new LongUMinus(e);
			else if(e.returnType().typeId() == Type.REAL)
				return new RealUMinus(e);
			else /* if(e.returnType().typeId() == Type.LONGREAL) // no check needed */
				return new DoubleUMinus(e);
		}
		else if(opCode == NOT && e.returnType().typeId() == Type.BOOLEAN)
			return new Not(e);
		else 
			throw new Error(Error.DOMAINERROR);
	}
	public static Expression Operation(int opCode, Expression e) throws Error {
		Expression result = DoOperation(opCode, e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	private static Expression DoOperation(int opCode, Expression e1, Expression e2) throws Error {
		if(!e1.returnType().equals(e2.returnType())) {
			if(e1.returnType().canBeConvertedTo(e2.returnType())) {
				e1 = TypeCast(e2.returnType(), e1);
			}
			else if(e2.returnType().canBeConvertedTo(e1.returnType())) {
				e2 = TypeCast(e1.returnType(), e2);
			}
			else
				throw new Error(Error.DOMAINERROR);
		}
		if(opCode == PLUS && e1.returnType().isNumeric()) {
			if(e1.returnType().typeId() == Type.SHORTINT)
				return new ShortPlus(e1, e2);
			else if(e1.returnType().typeId() == Type.INTEGER)
				return new IntPlus(e1, e2);
			else if(e1.returnType().typeId() == Type.LONGINT)
				return new LongPlus(e1, e2);
			else if(e1.returnType().typeId() == Type.REAL)
				return new RealPlus(e1, e2);
			else /* if(e.returnType().typeId() == Type.LONGREAL) // no check needed */
				return new DoublePlus(e1, e2);
		}
		else if(opCode == MINUS && e1.returnType().isNumeric()) {
			return Operation(PLUS, e1, Operation(MINUS, e2));
		}
		else if(opCode == TIMES && e1.returnType().isNumeric()) {
			if(e1.returnType().typeId() == Type.SHORTINT)
				return new ShortTimes(e1, e2);
			else if(e1.returnType().typeId() == Type.INTEGER)
				return new IntTimes(e1, e2);
			else if(e1.returnType().typeId() == Type.LONGINT)
				return new LongTimes(e1, e2);
			else if(e1.returnType().typeId() == Type.REAL)
				return new RealTimes(e1, e2);
			else /* if(e.returnType().typeId() == Type.LONGREAL) // no check needed */
				return new DoubleTimes(e1, e2);
		}
		else if(opCode == DIVIDE && e1.returnType().isNumeric()) {
			if(e1.returnType().typeId() == Type.SHORTINT)
				return new ShortDivide(e1, e2);
			else if(e1.returnType().typeId() == Type.INTEGER)
				return new IntDivide(e1, e2);
			else if(e1.returnType().typeId() == Type.LONGINT)
				return new LongDivide(e1, e2);
			else if(e1.returnType().typeId() == Type.REAL)
				return new RealDivide(e1, e2);
			else /* if(e.returnType().typeId() == Type.LONGREAL) // no check needed */
				return new DoubleDivide(e1, e2);
		}
		else if(opCode == DIV && e1.returnType().isInteger()) {
			if(e1.returnType().typeId() == Type.SHORTINT)
				return new ShortDiv(e1, e2);
			else if(e1.returnType().typeId() == Type.INTEGER)
				return new IntDiv(e1, e2);
			else /* if(e1.returnType().typeId() == Type.LONGINT) // no check needed */
				return new LongDiv(e1, e2);
		}
		else if(opCode == MOD && e1.returnType().isInteger()) {
			if(e1.returnType().typeId() == Type.SHORTINT)
				return new ShortMod(e1, e2);
			else if(e1.returnType().typeId() == Type.INTEGER)
				return new IntMod(e1, e2);
			else /* if(e1.returnType().typeId() == Type.LONGINT) // no check needed */
				return new LongMod(e1, e2);
		}
		else if(opCode == EQUAL) {
			if(e1.returnType().typeId() == Type.SHORTINT)
				return new ShortEqual(e1, e2);
			else if(e1.returnType().typeId() == Type.INTEGER)
				return new IntEqual(e1, e2);
			else if(e1.returnType().typeId() == Type.LONGINT)
				return new LongEqual(e1, e2);
			else if(e1.returnType().typeId() == Type.REAL)
				return new RealEqual(e1, e2);
			else if(e1.returnType().typeId() == Type.LONGREAL)
				return new DoubleEqual(e1, e2);
			else if(e1.returnType().typeId() == Type.CHAR)
				return new CharEqual(e1, e2);
			else if(e1.returnType().typeId() == Type.BOOLEAN)
				return new BooleanEqual(e1, e2);
			else if(e1.returnType().typeId() == Type.STRING)
				return new StringEqual(e1, e2);
			else
				throw new Error(Error.DOMAINERROR);
		}
		else if(opCode == NOTEQUAL) {
			return new Not(Operation(EQUAL, e1, e2));
		}
		else if(opCode == GREATER && e1.returnType().isNumeric()) {
			if(e1.returnType().typeId() == Type.SHORTINT)
				return new ShortGreater(e1, e2);
			else if(e1.returnType().typeId() == Type.INTEGER)
				return new IntGreater(e1, e2);
			else if(e1.returnType().typeId() == Type.LONGINT)
				return new LongGreater(e1, e2);
			else if(e1.returnType().typeId() == Type.REAL)
				return new RealGreater(e1, e2);
			else /* if(e.returnType().typeId() == Type.LONGREAL) // no check needed */
				return new DoubleGreater(e1, e2);
		}
		else if(opCode == LESS && e1.returnType().isNumeric()) {
			if(e1.returnType().typeId() == Type.SHORTINT)
				return new ShortLess(e1, e2);
			else if(e1.returnType().typeId() == Type.INTEGER)
				return new IntLess(e1, e2);
			else if(e1.returnType().typeId() == Type.LONGINT)
				return new LongLess(e1, e2);
			else if(e1.returnType().typeId() == Type.REAL)
				return new RealLess(e1, e2);
			else /* if(e.returnType().typeId() == Type.LONGREAL) // no check needed */
				return new DoubleLess(e1, e2);
		}
		else if(opCode == GREATEREQUAL && e1.returnType().isNumeric()) {
			return new Not(Operation(LESS, e1, e2));
		}		
		else if(opCode == LESSEQUAL && e1.returnType().isNumeric()) {
			return new Not(Operation(GREATER, e1, e2));
		}
		else if(opCode == AND && e1.returnType().typeId() == Type.BOOLEAN)
			return new And(e1, e2);
		else if(opCode == OR && e1.returnType().typeId() == Type.BOOLEAN)
			return new Or(e1, e2);
		else
			throw new Error(Error.DOMAINERROR);
	}
	public static Expression Operation(int opCode, Expression e1, Expression e2) throws Error {
		Expression result = DoOperation(opCode, e1, e2);
		if(e1 instanceof Constant && e2 instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	
	// Abs
	public static class ShortAbs extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.ShortInt;
		}
		public Object evaluate() throws Error {
			return (Short)(short)Math.abs((Short)e.evaluate());
		}
		public ShortAbs(Expression e) {
			this.e = e;
		}
	}
	public static class IntAbs extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			return (Integer)Math.abs((Integer)e.evaluate());
		}
		public IntAbs(Expression e) {
			this.e = e;
		}
	}
	public static class LongAbs extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			return (Long)Math.abs((Long)e.evaluate());
		}
		public LongAbs(Expression e) {
			this.e = e;
		}
	}
	public static class RealAbs extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			return (Float)Math.abs((Float)e.evaluate());
		}
		public RealAbs(Expression e) {
			this.e = e;
		}
	}
	public static class DoubleAbs extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.LongReal;
		}
		public Object evaluate() throws Error {
			return (Double)Math.abs((Double)e.evaluate());
		}
		public DoubleAbs(Expression e) {
			this.e = e;
		}
	}
	private static Expression DoAbs(Expression e) throws Error {
		if(e.returnType().isNumeric()) {
			if(e.returnType().typeId() == Type.SHORTINT)
				return new ShortAbs(e);
			else if(e.returnType().typeId() == Type.INTEGER)
				return new IntAbs(e);
			else if(e.returnType().typeId() == Type.LONGINT)
				return new LongAbs(e);
			else if(e.returnType().typeId() == Type.REAL)
				return new RealAbs(e);
			else /* if(e.returnType().typeId() == Type.LONGREAL) // no check needed */
				return new DoubleAbs(e);
		}
		else 
			throw new Error(Error.DOMAINERROR);
	}
	public static Expression Abs(Expression e) throws Error {
		Expression result = DoAbs(e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	
	// Ash & Lsh
	public static class ShortAsh extends Expression{
		private Expression e, n;
		public Type returnType() {
			return Type.ShortInt;
		}
		public Object evaluate() throws Error {
			long shift = (Long)n.evaluate();
			if(shift > 0) return (Short)(short)((Short)e.evaluate() << shift);
			else          return (Short)(short)((Short)e.evaluate() >> -shift);
		}
		public ShortAsh(Expression e, Expression n) {
			this.e = e;
			this.n = n;
		}
	}
	public static class IntAsh extends Expression{
		private Expression e, n;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			long shift = (Long)n.evaluate();
			if(shift > 0) return (Integer)((Integer)e.evaluate() << shift);
			else          return (Integer)((Integer)e.evaluate() >> -shift);
		}
		public IntAsh(Expression e, Expression n) {
			this.e = e;
			this.n = n;
		}
	}
	public static class LongAsh extends Expression{
		private Expression e, n;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			long shift = (Long)n.evaluate();
			if(shift > 0) return (Long)((Long)e.evaluate() << shift);
			else          return (Long)((Long)e.evaluate() >> -shift);
		}
		public LongAsh(Expression e, Expression n) {
			this.e = e;
			this.n = n;
		}
	}
	private static Expression DoAsh(Expression e, Expression n) throws Error {
		n = TypeCast(Type.LongInt, n);
		if(e.returnType().isInteger()) {
			if(e.returnType().typeId() == Type.SHORTINT)
				return new ShortAsh(e, n);
			else if(e.returnType().typeId() == Type.INTEGER)
				return new IntAsh(e, n);
			else /* if(e.returnType().typeId() == Type.LONGINT) // no check needed */
				return new LongAsh(e, n);
		}
		else 
			throw new Error(Error.DOMAINERROR);
	}
	public static Expression Ash(Expression e, Expression n) throws Error {
		Expression result = DoAsh(e, n);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	public static Expression Lsh(Expression e, Expression n) throws Error {
		return Ash(e, Operation(MINUS, n));
	}
	
	// Chr
	public static class Chr extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Char;
		}
		public Object evaluate() throws Error {
			long result = (long)(Long)e.evaluate();
			if(result < 0 || result > 255)
				throw new Error(Error.DOMAINERROR);
			return (Character)(char)result;
		}
		public Chr(Expression e) {
			this.e = e;
		}
	}
	private static Expression DoChr(Expression e) throws Error {
		e = TypeCast(Type.LongInt, e);
		return new Chr(e);
	}
	public static Expression Chr(Expression e) throws Error {
		Expression result = DoChr(e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	
	// Ord
	public static class Ord extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			return (Integer)(int)(char)(Character)e.evaluate();
		}
		public Ord(Expression e) {
			this.e = e;
		}
	}
	private static Expression DoOrd(Expression e) throws Error {
		if(e.returnType().typeId() != Type.CHAR)
			throw new Error(Error.DOMAINERROR);
		return new Ord(e);
	}
	public static Expression Ord(Expression e) throws Error {
		Expression result = DoOrd(e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	
	// Entier
	public static class Entier extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			return (long)Math.floor((Double)e.evaluate());
		}
		public Entier(Expression e) {
			this.e = e;
		}
	}
	private static Expression DoEntier(Expression e) throws Error {
		e = TypeCast(Type.LongReal, e);
		return new Entier(e);
	}
	public static Expression Entier(Expression e) throws Error {
		Expression result = DoEntier(e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	
	// Even
	private static Expression DoEven(Expression e) throws Error {
		e = TypeCast(Type.LongInt, e);
		return Operation(EQUAL, Operation(MOD, e, Constant(Type.LongInt, (Long)2l)), Constant(Type.LongInt, (Long)0l));
	}
	public static Expression Even(Expression e) throws Error {
		Expression result = DoEven(e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	public static Expression Odd(Expression e) throws Error {
		return Even(Operation(PLUS, e, Constant(Type.LongInt, (Long)1l)));
	}
	
	// Len
	public static class Len extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.LongInt;
		}
		public Object evaluate() throws Error {
			return (Long)(long)((ArrayList<Object>)e.evaluate()).size();
		}
		public Len(Expression e) {
			this.e = e;
		}
	}
	private static Expression DoLen(Expression e) throws Error {
		if(e.returnType().typeId() != Type.ARRAY)
			throw new Error(Error.DOMAINERROR);
		return new Len(e);
	}
	public static Expression Len(Expression e) throws Error {
		Expression result = DoLen(e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	
	// Long
	private static Expression DoLong(Expression e) throws Error {
		if(e.returnType().typeId() == Type.SHORTINT)
			return TypeCast(Type.Integer, e);
		else if(e.returnType().typeId() == Type.INTEGER)
			return TypeCast(Type.LongInt, e);
		else if(e.returnType().typeId() == Type.REAL)
			return TypeCast(Type.LongReal, e);
		else
			throw new Error(Error.DOMAINERROR);
	}
	public static Expression Long(Expression e) throws Error {
		Expression result = DoLong(e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	
	// Short
	public static class IntShort extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.ShortInt;
		}
		public Object evaluate() throws Error {
			return (Short)(short)(int)(Integer)e.evaluate();
		}
		public IntShort(Expression e) {
			this.e = e;
		}
	}
	public static class LongShort extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Integer;
		}
		public Object evaluate() throws Error {
			return (Integer)(int)(long)(Long)e.evaluate();
		}
		public LongShort(Expression e) {
			this.e = e;
		}
	}
	public static class DoubleShort extends Expression{
		private Expression e;
		public Type returnType() {
			return Type.Real;
		}
		public Object evaluate() throws Error {
			return (Float)(float)(double)(Double)e.evaluate();
		}
		public DoubleShort(Expression e) {
			this.e = e;
		}
	}
	private static Expression DoShort(Expression e) throws Error {
		if(e.returnType().typeId() == Type.INTEGER)
			return new IntShort(e);
		else if(e.returnType().typeId() == Type.LONGINT)
			return new LongShort(e);
		else if(e.returnType().typeId() == Type.LONGREAL)
			return new DoubleShort(e);
		else
			throw new Error(Error.DOMAINERROR);
	}
	public static Expression Short(Expression e) throws Error {
		Expression result = DoShort(e);
		if(e instanceof Constant)
			return new Constant(result.returnType(), result.evaluate());
		return result;
	}
	
	// Function Call
	public static class FunctionCall extends Expression{
		private int funcNo;
		private ArrayList<Expression> args;
		public Type returnType() {
			return Module.idents.get(funcNo).getType().funcReturnType();
		}
		public Object evaluate() throws Error {
			ArrayList<Integer> indexes = new ArrayList<Integer>();
			int indexesPos = 0;
			Identifier func = Module.idents.get(funcNo);
			FunctionValue funcValue = (FunctionValue)func.getValue();
			Object returnValue = null;
			if(funcValue.isDefault) {
				if(funcValue.name.equals("Out.Ln")) {
					System.out.println();
				}
				else if(funcValue.name.equals("Out.String") || funcValue.name.equals("Out.Char")) {
					System.out.print(args.get(0).evaluate());
				}
				else if(funcValue.name.startsWith("Out.") && !funcValue.name.endsWith("Format")) {
					String s = args.get(0).evaluate().toString();
					int n = (Integer)args.get(1).evaluate();
					while(s.length() < n)
						s = " " + s;
					System.out.print(s);
				}
				else if(funcValue.name.startsWith("Out.") && funcValue.name.endsWith("Format")) {
					System.out.printf((String)args.get(0).evaluate(), args.get(1).evaluate());
				}
				else if(funcValue.name.equals("In.Open")) {
					// do nothing 
				}
				else if(funcValue.name.startsWith("In.")) {
					int varNo = ((Expression.Variable)args.get(0)).getVarNo();
					Expression index = ((Expression.Variable)args.get(0)).getIndex();
					if(funcValue.name.equals("In.Char"))
						Statement.Assignment(varNo, index, Constant(Type.Char, In.Char())).run();
					else if(funcValue.name.equals("In.ShortInt"))
						Statement.Assignment(varNo, index, Constant(Type.ShortInt, In.Short())).run();
					else if(funcValue.name.equals("In.Integer") || funcValue.name.equals("In.Int"))
						Statement.Assignment(varNo, index, Constant(Type.Integer, In.Int())).run();
					else if(funcValue.name.equals("In.LongInt"))
						Statement.Assignment(varNo, index, Constant(Type.LongInt, In.Long())).run();
					else if(funcValue.name.equals("In.Real"))
						Statement.Assignment(varNo, index, Constant(Type.Real, In.Float())).run();
					else if(funcValue.name.equals("In.LongReal"))
						Statement.Assignment(varNo, index, Constant(Type.LongReal, In.Double())).run();
					else if(!funcValue.name.equals("In.Open"))
						throw new Error(Error.DOMAINERROR);
				}
				else if(funcValue.name.equals("Math.PI"))
					returnValue = (float)Math.PI;
				else if(funcValue.name.equals("Math.L_PI"))
					returnValue = (double)Math.PI;
				else if(funcValue.name.equals("Math.E"))
					returnValue = (float)Math.E;
				else if(funcValue.name.equals("Math.L_E"))
					returnValue = (double)Math.E;
				else if(funcValue.name.equals("Math.Exp"))
					returnValue = (float)Math.exp((Float)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.Ln"))
					returnValue = (float)Math.log((Float)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.Sin"))
					returnValue = (float)Math.sin((Float)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.Cos"))
					returnValue = (float)Math.cos((Float)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.Atan"))
					returnValue = (float)Math.atan((Float)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.Atan2"))
					returnValue = (float)Math.atan2((Float)args.get(0).evaluate(), (Float)args.get(1).evaluate());
				else if(funcValue.name.equals("Math.LongExp"))
					returnValue = (double)Math.exp((Double)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.LongLn"))
					returnValue = (double)Math.log((Double)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.LongSin"))
					returnValue = (double)Math.sin((Double)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.LongCos"))
					returnValue = (double)Math.cos((Double)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.LongAtan"))
					returnValue = (double)Math.atan((Double)args.get(0).evaluate());
				else if(funcValue.name.equals("Math.LongAtan2"))
					returnValue = (double)Math.atan2((Double)args.get(0).evaluate(), (Float)args.get(1).evaluate());
				else 
					throw new Error(Error.DOMAINERROR);
			}
			else {
				ArrayList<Object> params = new ArrayList<Object>();
				for(int i = 0; i < func.getType().funcParamCount(); i++) {
					if(func.getType().funcIsVarParam(i)) {
						Identifier var = Module.idents.get(((Expression.Variable)args.get(i)).getVarNo());
						if(((Expression.Variable)args.get(i)).getIndex() != null) {
							indexes.add((int)(long)(Long)((Expression.Variable)args.get(i)).getIndex().evaluate());
							Object value = ((ArrayList<Object>)var.getValue()).get(indexes.get(indexes.size() - 1));
							//Module.idents.get(funcValue.locals.get(i)).pushValue(value);
							params.add(value);
						}
						else {
							if(var.getType().typeId() == Type.ARRAY)
								//Module.idents.get(funcValue.locals.get(i)).pushValue(((ArrayList<Object>)args.get(i).evaluate()).clone());
								params.add(((ArrayList<Object>)args.get(i).evaluate()).clone());
							else
								//Module.idents.get(funcValue.locals.get(i)).pushValue(args.get(i).evaluate());
								params.add(args.get(i).evaluate());
						}
					}
					else
						//Module.idents.get(funcValue.locals.get(i)).pushValue(args.get(i).evaluate());
						params.add(args.get(i).evaluate());
				}
				for(int i = 0; i < func.getType().funcParamCount(); i++) {
					Module.idents.get(funcValue.locals.get(i)).pushValue(params.get(i));
				}
				for(int i = func.getType().funcParamCount(); i < funcValue.locals.size(); i++)
					if(!Module.idents.get(funcValue.locals.get(i)).isConst())
						Module.idents.get(funcValue.locals.get(i)).pushDefaultValue();
				try {
					funcValue.body.run();
				} catch (Error.Return e) {
					returnValue = e.getValue();
				}
				for(int i = 0; i < func.getType().funcParamCount(); i++) {
					if(func.getType().funcIsVarParam(i)) {
						Identifier var = Module.idents.get(((Expression.Variable)args.get(i)).getVarNo());
						Object value = null;
						value = Module.idents.get(funcValue.locals.get(i)).getValue();
						Module.idents.get(funcValue.locals.get(i)).popValue();
						if(((Expression.Variable)args.get(i)).getIndex() != null)
							((ArrayList<Object>)var.getValue()).set(indexes.get(indexesPos++), value);
						else
							var.setValue(value);
					}
					else
						Module.idents.get(funcValue.locals.get(i)).popValue();
				}
				for(int i = func.getType().funcParamCount(); i < funcValue.locals.size(); i++)
					if(!Module.idents.get(funcValue.locals.get(i)).isConst()) 
						Module.idents.get(funcValue.locals.get(i)).popValue();
			}
			return returnValue;
		}
		public FunctionCall(int funcNo, ArrayList<Expression> args) {
			this.funcNo = funcNo;
			this.args = args;
		}
	}
	private static Expression.FunctionCall DoFunctionCall(int funcNo, ArrayList<Expression> args) throws Error {
		Type funcType = Module.idents.get(funcNo).getType();
		for(int i = 0; i < args.size(); i++) {
			if(funcType.funcIsVarParam(i) && !args.get(i).returnType().equals(funcType.funcParamType(i))) 
				throw new Error(Error.COMPILEERROR, "Cannot pass variable of different type as a var paramener");
			args.set(i, TypeCast(funcType.funcParamType(i), args.get(i)));
		}
		return new FunctionCall(funcNo, args);
	}
	public static Expression.FunctionCall FunctionCall(int funcNo, ArrayList<Expression> args) throws Error {
		return DoFunctionCall(funcNo, args);
	}
	
	public abstract Type returnType();
	public abstract Object evaluate() throws Error;
}
