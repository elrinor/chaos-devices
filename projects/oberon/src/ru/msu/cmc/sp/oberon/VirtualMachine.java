package ru.msu.cmc.sp.oberon;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Stack;
import antlr.SemanticException;


public class VirtualMachine {
	public Context context;
	public Stack<TypedValue> stack;
	
	public static Type invokeReturnType(Variable invoke, Type argType) throws SemanticException {
		String name = (String)invoke.value().value;
		if(name.startsWith("Out.") || name.startsWith("In."))
			return Type.VoidType;
		VirtualMachine v = new VirtualMachine();
		if(argType != null) {
			v.stack.push(argType.defaultValue());
			v.stack.push(argType.defaultValue());
		}
		v.Invoke((String)invoke.value().value);
		return v.stack.pop().type;
	}
	
	public VirtualMachine(Context context) {
		this();
		this.context = context;
	}
	private VirtualMachine() {
		this.stack = new Stack<TypedValue>();
	}
	
	public void Invoke(String proc) throws SemanticException {
		try {
			if(proc.equals("LONG")) {
				TypedValue value = stack.pop();
				if(value.type.type == BuiltInType.SHORTINT)
					stack.push(new TypedValue(Type.IntegerType, (Integer)(int)(short)(Short)value.value));
				else if(value.type.type == BuiltInType.INTEGER)
					stack.push(new TypedValue(Type.LongIntType, (Long)(long)(int)(Integer)value.value));
				else if(value.type.type == BuiltInType.REAL)
					stack.push(new TypedValue(Type.LongRealType, (Double)(double)(float)(Float)value.value));
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "LONG() is not applicable to argument of type " + value.type.type);
			} else if(proc.equals("SHORT")) {
				TypedValue value = stack.pop();
				if(value.type.type == BuiltInType.INTEGER)
					stack.push(new TypedValue(Type.ShortIntType, (Short)(short)(int)(Integer)value.value));
				else if(value.type.type == BuiltInType.LONGINT)
					stack.push(new TypedValue(Type.IntegerType, (Integer)(int)(long)(Long)value.value));
				else if(value.type.type == BuiltInType.LONGREAL)
					stack.push(new TypedValue(Type.RealType, (Float)(float)(double)(Double)value.value));
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "SHORT() is not applicable to argument of type " + value.type.type);
			} else if(proc.equals("ASH")) {
				int amount = (int)(long)(Long)stack.pop().ConvertTo(Type.LongIntType).value;
				if(amount < 0)
				{
					stack.push(new TypedValue(Type.LongIntType, (Long)(long)-amount));
					Invoke("LSH");
				}
				else
				{
					TypedValue value = stack.pop();
					if(value.type.type == BuiltInType.SHORTINT)
						stack.push(new TypedValue(Type.LongIntType, (Long)(long)((short)(Short)value.value << amount)));
					else if(value.type.type == BuiltInType.INTEGER)
						stack.push(new TypedValue(Type.LongIntType, (Long)(long)((int)(Integer)value.value << amount)));
					else if(value.type.type == BuiltInType.LONGINT)
						stack.push(new TypedValue(Type.LongIntType, (Long)(long)((long)(Long)value.value << amount)));
					else
						throw new RuntimeError(Error.TYPEMISMATCH, "ASH() is not applicable to argument of type " + value.type.type);
				}
			} else if(proc.equals("LSH")) {
				int amount = (int)(long)(Long)stack.pop().ConvertTo(Type.LongIntType).value;
				if(amount < 0)
				{
					stack.push(new TypedValue(Type.LongIntType, (Long)(long)-amount));
					Invoke("ASH");
				}
				else
				{
					TypedValue value = stack.pop();
					if(value.type.type == BuiltInType.SHORTINT)
						stack.push(new TypedValue(Type.LongIntType, (Long)(long)((short)(Short)value.value >> amount)));
					else if(value.type.type == BuiltInType.INTEGER)
						stack.push(new TypedValue(Type.LongIntType, (Long)(long)((int)(Integer)value.value >> amount)));
					else if(value.type.type == BuiltInType.LONGINT)
						stack.push(new TypedValue(Type.LongIntType, (Long)(long)((long)(Long)value.value >> amount)));
					else
						throw new RuntimeError(Error.TYPEMISMATCH, "LSH() is not applicable to argument of type " + value.type.type);
				}
			} else if(proc.equals("CHR")) {
				long charNum = (long)(Long)Type.cloneValue(stack.pop()).ConvertTo(Type.LongIntType).value;
				if(charNum > 255 || charNum < 0)
					throw new RuntimeError(Error.TYPEMISMATCH, "CHR() failed");
				stack.push(new TypedValue(Type.CharType, (Character)(char)charNum));
			} else if(proc.equals("ORD")) {
				stack.push(new TypedValue(Type.ShortIntType, (Short)(short)(char)(Character)Type.cloneValue(stack.pop()).ConvertTo(Type.CharType).value));
			} else if(proc.equals("ENTIER")) {
				double dv = (double)(Double)Type.cloneValue(stack.pop()).ConvertTo(Type.LongRealType).value;
				long v = (long)dv;
				if(v < 0 && v != dv)
					v--;
				stack.push(new TypedValue(Type.LongIntType, (Long)v));
			} else if(proc.equals("LEN")) {
				TypedValue value = stack.pop();
				if(value.type.type == BuiltInType.STRING)
					stack.push(new TypedValue(Type.LongIntType, (Long)(long)((String)value.value).length()));
				else if(value.type.type == BuiltInType.ARRAY)
					stack.push(new TypedValue(Type.LongIntType, (Long)(long)value.type.arraySize()));
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "LEN() is not applicable to argument of type " + value.type.type);
			} else if(!proc.equals("Out.Ln") && proc.startsWith("Out.") && !proc.endsWith("Format")) {
				int fldWidth = 0;
				if(proc.equals("Out.ShortInt") || proc.equals("Out.Int") || proc.equals("Out.Integer") || proc.equals("Out.LongInt") || proc.equals("Out.Real") || proc.equals("Out.LongReal"))
					fldWidth = (Integer)stack.pop().ConvertTo(Type.IntegerType).value;
				String value = stack.pop().value.toString();
				while(value.length() < fldWidth)
					value = " " + value;
				System.out.print(value);
				stack.push(TypedValue.VoidTypedValue);
			} else if(proc.startsWith("Out.") && proc.endsWith("Format")) {
				Object[] args = new Object[1];
				args[0] = stack.pop().value;
				String fmt = (String) stack.pop().ConvertTo(Type.StringType).value;
				System.out.printf(fmt, args);
				stack.push(TypedValue.VoidTypedValue);
			} else if(proc.equals("Out.Ln")) {
				System.out.print(System.getProperty("line.separator"));
				stack.push(TypedValue.VoidTypedValue);
			} else if(proc.equals("In.Open")) { // Do nothing
				stack.push(TypedValue.VoidTypedValue);
			} else if(proc.equals("In.Char")) {
				if(stack.lastElement().type.type == BuiltInType.CHAR)
					stack.lastElement().value = (Character)Input.readChar();
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "In.Char() is not applicable to argument of type " + stack.lastElement().type.type);
			} else if(proc.equals("In.ShortInt")) {
				if(stack.lastElement().type.type == BuiltInType.SHORTINT)
					stack.lastElement().value = (Short)Input.readShort();
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "In.ShortInt() is not applicable to argument of type " + stack.lastElement().type.type);
			} else if(proc.equals("In.Int")) {
				if(stack.lastElement().type.type == BuiltInType.INTEGER)
					stack.lastElement().value = (Integer)Input.readInt();
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "In.Int() is not applicable to argument of type " + stack.lastElement().type.type);
			} else if(proc.equals("In.LongInt")) {
				if(stack.lastElement().type.type == BuiltInType.LONGINT)
					stack.lastElement().value = (Long)Input.readLong();
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "In.LongInt() is not applicable to argument of type " + stack.lastElement().type.type);
			} else if(proc.equals("In.Real")) {
				if(stack.lastElement().type.type == BuiltInType.REAL)
					stack.lastElement().value = (Float)Input.readFloat();
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "In.Real() is not applicable to argument of type " + stack.lastElement().type.type);
			} else if(proc.equals("In.LongReal")) {
				if(stack.lastElement().type.type == BuiltInType.LONGREAL)
					stack.lastElement().value = (Double)Input.readDouble();
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "In.LongReal() is not applicable to argument of type " + stack.lastElement().type.type);
			} else if(proc.equals("In.String")) {
				if(stack.lastElement().type.type == BuiltInType.STRING)
					stack.lastElement().value = Input.readString();
				else
					throw new RuntimeError(Error.TYPEMISMATCH, "In.String() is not applicable to argument of type " + stack.lastElement().type.type);
			} else if(proc.equals("Math.PI")) {
				stack.push(new TypedValue(Type.RealType, (Float)(float)Math.PI));
			} else if(proc.equals("Math.L_PI")) {
				stack.push(new TypedValue(Type.LongRealType, (Double)Math.PI));
			} else if(proc.equals("Math.E")) {
				stack.push(new TypedValue(Type.RealType, (Float)(float)Math.E));
			} else if(proc.equals("Math.L_E")) {
				stack.push(new TypedValue(Type.LongRealType, (Double)Math.E));
			} else if(proc.equals("Math.Exp")) {
				stack.push(new TypedValue(Type.RealType, (Float)(float)Math.exp((Float)stack.pop().ConvertTo(Type.RealType).value)));
			} else if(proc.equals("Math.Ln")) {
				stack.push(new TypedValue(Type.RealType, (Float)(float)Math.log((Float)stack.pop().ConvertTo(Type.RealType).value)));
			} else if(proc.equals("Math.Sin")) {
				stack.push(new TypedValue(Type.RealType, (Float)(float)Math.sin((Float)stack.pop().ConvertTo(Type.RealType).value)));
			} else if(proc.equals("Math.Cos")) {
				stack.push(new TypedValue(Type.RealType, (Float)(float)Math.cos((Float)stack.pop().ConvertTo(Type.RealType).value)));
			} else if(proc.equals("Math.Atan")) {
				stack.push(new TypedValue(Type.RealType, (Float)(float)Math.atan((Float)stack.pop().ConvertTo(Type.RealType).value)));
			} else if(proc.equals("Math.Atan2")) {
				Float v2 = (Float)stack.pop().ConvertTo(Type.RealType).value;
				Float v1 = (Float)stack.pop().ConvertTo(Type.RealType).value;
				stack.push(new TypedValue(Type.RealType, (Float)(float)Math.atan2(v1, v2)));
			} else if(proc.equals("Math.LongExp")) {
				stack.push(new TypedValue(Type.LongRealType, (Double)(double)Math.exp((Double)stack.pop().ConvertTo(Type.LongRealType).value)));
			} else if(proc.equals("Math.LongLn")) {
				stack.push(new TypedValue(Type.LongRealType, (Double)(double)Math.log((Double)stack.pop().ConvertTo(Type.LongRealType).value)));
			} else if(proc.equals("Math.LongSin")) {
				stack.push(new TypedValue(Type.LongRealType, (Double)(double)Math.sin((Double)stack.pop().ConvertTo(Type.LongRealType).value)));
			} else if(proc.equals("Math.LongCos")) {
				stack.push(new TypedValue(Type.LongRealType, (Double)(double)Math.cos((Double)stack.pop().ConvertTo(Type.LongRealType).value)));
			} else if(proc.equals("Math.LongAtan")) {
				stack.push(new TypedValue(Type.LongRealType, (Double)(double)Math.atan((Double)stack.pop().ConvertTo(Type.LongRealType).value)));
			} else if(proc.equals("Math.LongAtan2")) {
				Double v2 = (Double)stack.pop().ConvertTo(Type.LongRealType).value;
				Double v1 = (Double)stack.pop().ConvertTo(Type.LongRealType).value;
				stack.push(new TypedValue(Type.LongRealType, (Double)(double)Math.atan2(v1, v2)));
			} else if(proc.equals("Math.Random")) {
				stack.push(new TypedValue(Type.LongIntType, (Long)(long)(Math.random() * ((Double)stack.pop().ConvertTo(Type.LongRealType).value))));
			} else
				throw new RuntimeError(Error.TYPEMISMATCH, "Unknown invoke \"" + proc + "\"");
		} catch (IOException e) {
			throw new RuntimeError(Error.INPUTERROR, e.getMessage());
		} catch (NumberFormatException e) {
			throw new RuntimeError(Error.INPUTERROR, e.getMessage());
		}
	}
	
	public void Run() throws Exception {
		Variable procVar = context.variables.get(context.mainId);
		ArrayList<Variable> vars = ((CodeBlock)procVar.value().value).locals;
		for(int i = 0; i < vars.size(); i++)
			if(!vars.get(i).isConst)
			{
				vars.get(i).values.clear();
				vars.get(i).values.push(vars.get(i).type.defaultValue());
			}
		int pos = 0;
		TypedValue v1, v2;
		boolean Terminated = false;
		int ExitCode = -1;
		AsmStatement asm = null;
		while(!Terminated) {
			asm = ((CodeBlock)procVar.value().value).statements.get(pos);
			try {
				switch (asm.type) {
				case JMP:
					pos = (Integer) asm.f - 1;
					break;
				case JMPNZ:
					if((Boolean)stack.pop().ConvertTo(Type.BooleanType).value)
						pos = (Integer) asm.f - 1;
					break;
				case BINOP:
					v2 = stack.pop();
					v1 = stack.pop();
					stack.push(((Operation) asm.f).Perform(v1, v2));
					break;
				case UNOP:
					stack.push(((Operation) asm.f).Perform(stack.pop()));
					break;
				case PUSH:
					stack.push((TypedValue) asm.f);
					break;
				case PUSHVAR:
					stack.push(context.variables.get((Integer) asm.f).value());
					break;
				case POP:
					stack.pop();
					break;
				case CALL:
					Variable newProcVar = context.variables.get((Integer) asm.f);
					if(newProcVar.type.isInvoke())
						Invoke((String)newProcVar.value().value);
					else
					{
						for(int i = newProcVar.value().type.paramTypes().size() - 1; i >= 0; i--)
							if(newProcVar.type.isRefParam().get(i))
								((CodeBlock)newProcVar.value().value).locals.get(i).values.push(stack.pop());
							else
								((CodeBlock)newProcVar.value().value).locals.get(i).values.push(Type.cloneValue(stack.pop()).ConvertTo(((CodeBlock)newProcVar.value().value).locals.get(i).type));
						stack.push(new TypedValue(Type.IntegerType, (Integer) pos));
						stack.push(new TypedValue(Type.IntegerType, (Integer) procVar.id));
						pos = -1;
						vars = ((CodeBlock)newProcVar.value().value).locals;
						procVar = newProcVar;
						for(int i = newProcVar.value().type.paramTypes().size(); i < vars.size(); i++)
							if(!vars.get(i).isConst)
								vars.get(i).values.push(vars.get(i).type.defaultValue());
					}
					break;
				case RET:
					vars = ((CodeBlock)procVar.value().value).locals;
					for(int i = 0; i < vars.size(); i++)
						if(!vars.get(i).isConst)
							vars.get(i).values.pop();
					TypedValue result = stack.pop();
					procVar = context.variables.get((Integer)stack.pop().value);
					pos = (Integer)stack.pop().value;
					stack.push(result);
					break;
				case EXIT:
					ExitCode = (int)(long)(Long)stack.pop().ConvertTo(Type.LongIntType).value;
					Terminated = true;
					break;
				case DUP:
					stack.push(stack.get(stack.size() - 1 - (Integer) asm.f));
					break;
				}
			} catch (RuntimeError e) {
				throw new RuntimeError(e, context.fileName + ":" + asm.sourceLine + ":" + asm.sourceCol + ":" + e.getMessage());
			} catch (Exception e) {
				throw new Exception(context.fileName + ":" + asm.sourceLine + ":" + asm.sourceCol + ":" + e.getMessage(), e);
			} 
			pos++;
		}
	}
	
}
