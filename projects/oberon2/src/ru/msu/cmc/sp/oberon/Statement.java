package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;

import ru.msu.cmc.sp.oberon.Error.Break;
import ru.msu.cmc.sp.oberon.Error.Continue;

public abstract class Statement{
	// Sequence
	public static class Sequence extends Statement{
		public ArrayList<Statement> statements;
		public void run() throws Error {
			for(int i = 0; i < statements.size(); i++)
				statements.get(i).run();
		}
		public Sequence() {
			this.statements = new ArrayList<Statement>();
		}
	}
	public static Sequence Sequence() {
		return new Sequence();
	}
	public static Sequence Sequence(Statement s1, Statement s2) {
		Sequence s = Sequence();
		s.statements.add(s1);
		s.statements.add(s2);
		return s;
	}
	public static Sequence Sequence(Statement s1, Statement s2, Statement s3) {
		Sequence s = Sequence();
		s.statements.add(s1);
		s.statements.add(s2);
		s.statements.add(s3);
		return s;
	}
	
	// Assignment
	public static class Assignment extends Statement{
		private int varNo;
		private Expression index;
		private Expression expr;
		public void run() throws Error {
			if(index == null) {
				if(Module.idents.get(varNo).getType().typeId() == Type.ARRAY)
					Module.idents.get(varNo).setValue(((ArrayList<Object>)expr.evaluate()).clone());
				else
					Module.idents.get(varNo).setValue(expr.evaluate());
			}
			else
				((ArrayList<Object>)Module.idents.get(varNo).getValue()).set((int)(long)(Long)index.evaluate(), expr.evaluate());
		}
		public Assignment(int varNo, Expression index, Expression expr) {
			this.varNo = varNo;
			this.index = index;
			this.expr = expr;
		}
	}
	public static Assignment Assignment(int varNo, Expression index, Expression expr) throws Error {
		Type varType = null;
		if(index != null && Module.idents.get(varNo).getType().typeId() != Type.ARRAY)
			throw new Error(Error.COMPILEERROR, "Cannot index non-array variable");
		if(index != null)
			varType = Module.idents.get(varNo).getType().arrayType();
		else
			varType = Module.idents.get(varNo).getType();
		if(index != null)
			index = Expression.TypeCast(Type.LongInt, index);
		expr = Expression.TypeCast(varType, expr);
		return new Assignment(varNo, index, expr);
	}
	public static Assignment Assignment(int varNo, Expression expr) throws Error {
		return Assignment(varNo, null, expr);
	}
	
	// Inc & Dec
	public static Statement Inc(int varNo, Expression index, Expression expr) throws Error {
		if(expr == null)
			expr = new Expression.Constant(Type.ShortInt, (Short)(short)1);
		Type varType = null;
		if(index != null)
			varType = Module.idents.get(varNo).getType().arrayType();
		else
			varType = Module.idents.get(varNo).getType();
		if(!varType.isInteger())
			throw new Error(Error.COMPILEERROR, "Cannot INC/DEC non-integer variable");
		return Assignment(varNo, index, Expression.Operation(Expression.PLUS, Expression.Variable(varNo, index), expr));
	}
	public static Statement Dec(int varNo, Expression index, Expression expr) throws Error {
		if(expr == null)
			expr = new Expression.Constant(Type.ShortInt, (Short)(short)1);
		return Inc(varNo, index, Expression.Operation(Expression.MINUS, expr));
	}
	
	// Procedure Call
	public static class ProcedureCall extends Statement{
		private Expression funcCall;
		public void run() throws Error {
			funcCall.evaluate();
		}
		public ProcedureCall(int procNo, ArrayList<Expression> args) throws Error {
			funcCall = Expression.FunctionCall(procNo, args);
		}
		public ProcedureCall(Expression.FunctionCall functionCall){
			funcCall = functionCall;
		}
	}
	public static ProcedureCall ProcedureCall(int procNo, ArrayList<Expression> args) throws Error {
		return new ProcedureCall(procNo, args);
	}
	public static ProcedureCall ProcedureCall(Expression.FunctionCall functionCall){
		return new ProcedureCall(functionCall);
	}

	// If-else
	public static class If extends Statement{
		private Expression expr;
		private Statement thenBlock, elseBlock;
		public void run() throws Error {
			if((Boolean)expr.evaluate())
				thenBlock.run();
			else if(elseBlock != null)
				elseBlock.run();
		}
		public If(Expression expr, Statement thenBlock, Statement elseBlock) {
			this.expr = expr;
			this.thenBlock = thenBlock;
			this.elseBlock = elseBlock;
		}
	}
	public static If If(Expression expr, Statement thenBlock, Statement elseBlock) throws Error {
		if(expr.returnType().typeId() != Type.BOOLEAN)
			throw new Error(Error.COMPILEERROR, "Boolean expression expected");
		return new If(expr, thenBlock, elseBlock);
	}
	public static If If(Expression expr, Statement thenBlock) throws Error {
		return If(expr, thenBlock, null);
	}
	
	// Loop
	public static class Loop extends Statement{
		private Statement block;
		public void run() throws Error{
			while(true) {
				try {
					block.run();
				} catch (Error.Continue e) {
					continue;
				} catch (Error.Break e) {
					break;
				}
			}
		}
		public Loop(Statement block) {
			this.block = block;
		}
	}
	public static Loop Loop(Statement block) {
		return new Loop(block);
	}
	public static Loop While(Statement block, Expression expr) throws Error {
		return Loop(Sequence(If(expr, Nop(), Break()), block));
	}
	public static Loop Repeat(Statement block, Expression expr) throws Error {
		return Loop(Sequence(block, If(expr, Break())));
	}
	public static Statement For(int controlVarNo, Expression from, Expression to, Expression by, Statement block) throws Error {
		Sequence result = new Sequence();
		if(!by.returnType().isInteger())
			throw new Error(Error.COMPILEERROR, "For loop step must be integer");
		long byValue = (Long)Expression.TypeCast(Type.LongInt, by).evaluate(); 
		if(byValue == 0)
			throw new Error(Error.COMPILEERROR, "For loop step must be nonzero");
		if(!Module.idents.get(controlVarNo).getType().isInteger())
			throw new Error(Error.COMPILEERROR, "For loop control variable must be of integer type");
		result.statements.add(Assignment(controlVarNo, from));
		Identifier tmp = new Identifier(Type.LongInt);
		Module.addTempIdent(tmp);
		result.statements.add(Assignment(tmp.getNo(), to));
		Statement body = Sequence(block, Assignment(controlVarNo, 
			Expression.Operation(Expression.PLUS, Expression.Variable(controlVarNo, null), by)));
		if(byValue > 0)
			result.statements.add(While(body, Expression.Operation(Expression.LESSEQUAL, Expression.Variable(controlVarNo, null), Expression.Variable(tmp.getNo(), null))));
		else
			result.statements.add(While(body, Expression.Operation(Expression.GREATEREQUAL, Expression.Variable(controlVarNo, null), Expression.Variable(tmp.getNo(), null))));
		return result;
	}
	
	
	// Break	
	public static class Break extends Statement{
		public void run() throws Error.Break{
			throw new Error.Break();
		}
	}
	private static Break Break = new Break();
	public static Break Break() {
		return Break;
	}
	
	// Continue	
	public static class Continue extends Statement{
		public void run() throws Error.Continue{
			throw new Error.Continue();
		}
	}
	private static Continue Continue = new Continue();
	public static Continue Continue() {
		return Continue;
	}
	
	// Return
	public static class Return extends Statement{
		private Expression e;
		public void run() throws Error{
			throw new Error.Return(e.evaluate());
		}
		public Return(Expression e) {
			this.e = e;
		}
	}
	public static Return Return(Expression e) throws Error {
		return new Return(Expression.TypeCast(Module.idents.get(Module.currentFuncNo()).getType().funcReturnType(), e));
	}
	
	// Nop
	public static class Nop extends Statement{
		public void run(){
		}
	}
	private static Nop Nop = new Nop();
	public static Nop Nop() {
		return Nop;
	}
	
	// Halt
	public static class Halt extends Statement{
		private int code;
		public void run() throws Error{
			throw new Error.Halt(code);
		}
		public Halt(int code) {
			this.code = code;
		}
	}
	public static Halt Halt(int code) {
		return new Halt(code);
	}
	
	public abstract void run() throws Error;
}
