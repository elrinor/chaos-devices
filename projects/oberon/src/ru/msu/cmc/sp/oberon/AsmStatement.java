package ru.msu.cmc.sp.oberon;

import antlr.Token;

public class AsmStatement {
	enum AsmStatementType {
		JMP,     // Absolute jump    [f: Integer]
		JMPNZ,   // Jump if true     [f: Integer]
		BINOP,   // Binary op        [f: Operation]
		UNOP,    // Unary op         [f: Operation]
		PUSH,    // Push to stack    [f: TypedValue]
		PUSHVAR, // Push Variable    [f: Integer]
		POP,     // Pop              []
		CALL,    // Call             [f: Integer]
		RET,     // Return from a subroutine
		EXIT,    // Exit aka Halt
		DUP;     // Duplicate value on stack           [f: Integer]
	}
	Object f;
	AsmStatementType type;
	int sourceLine;
	int sourceCol;
	
	public AsmStatement(AsmStatementType type, Object arg, int sourceLine, int sourceCol) {
		this.type = type;
		this.f = arg;
		this.sourceCol = sourceCol;
		this.sourceLine = sourceLine;
	}
	
	public AsmStatement(AsmStatementType type, Object arg, Token sourceToken) {
		this(type, arg, sourceToken.getLine(), sourceToken.getColumn());
	}
	
	public String toString() {
		if(type != AsmStatementType.POP && type != AsmStatementType.RET && type != AsmStatementType.EXIT)
			return type.toString() + " " + f.toString();
		else
			return type.toString();
	}

	/*
	private static final AsmStatement PopStatement = new AsmStatement(AsmStatementType.POP, null);
	private static final AsmStatement RetStatement = new AsmStatement(AsmStatementType.RET, null);
	private static final AsmStatement ExitStatement = new AsmStatement(AsmStatementType.EXIT, null);
	*/
	
	static public AsmStatement Jmp(int where) {
		return new AsmStatement(AsmStatementType.JMP, (Integer)where, 0, 0);
	}
	/*static public AsmStatement RelJmp(int where) {
		return new AsmStatement(AsmStatementType.RELJMP, (Integer)where);
	}*/
	static public AsmStatement JmpNZ(int where, Token sourceToken) {
		return new AsmStatement(AsmStatementType.JMPNZ, (Integer)where, sourceToken);
	}
	static public AsmStatement BinOp(Operation op, Token sourceToken) {
		return new AsmStatement(AsmStatementType.BINOP, op, sourceToken);
	}
	static public AsmStatement UnOp(Operation op, Token sourceToken) {
		return new AsmStatement(AsmStatementType.UNOP, op, sourceToken);
	}
	static public AsmStatement Push(TypedValue value) {
		return new AsmStatement(AsmStatementType.PUSH, value, 0, 0);
	}
	static public AsmStatement PushVar(int variableId, Token sourceToken) {
		return new AsmStatement(AsmStatementType.PUSHVAR, variableId, sourceToken);
	}
	static public AsmStatement Pop(Token sourceToken) {
		return new AsmStatement(AsmStatementType.POP, null, sourceToken);
	}
	static public AsmStatement Call(int what, Token sourceToken) {
		return new AsmStatement(AsmStatementType.CALL, (Integer)what, sourceToken);
	}
	static public AsmStatement Ret(Token sourceToken) {
		return new AsmStatement(AsmStatementType.RET, null, sourceToken);
	}
	static public AsmStatement Exit(Token sourceToken) {
		return new AsmStatement(AsmStatementType.EXIT, null, sourceToken);
	}
	static public AsmStatement Dup(int n, Token sourceToken) {
		return new AsmStatement(AsmStatementType.DUP, (Integer)n, sourceToken);
	}
}

// What Call Does:
// Store pops in local vars 
// Push [position in block]
// Push [current procedure id]
// Push LocalVar stacks

// What Ret Does:
// Pop LocalVar stacks
// Pop  [result]
// Pop  [ret proc id]
// Pop  [ret proc pos]
// Push [result] 
// !IDEA: in procedure - return NULL
