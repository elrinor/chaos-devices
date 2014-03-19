/* $Id: oberon.g 11 2006-04-14 07:06:11Z cher $ */
header {
package ru.msu.cmc.sp.oberon;
import java.util.ArrayList;
}

class Oberon2Scanner extends Lexer;
options {
	k = 2;
	defaultErrorHandler = false;
}
tokens {
	INTEGER;
	REAL;
	CHAR;
}

STAR: "*" ;
DOT: "." ;
EQUAL: "=" ;
COMMA: "," ;
SEMICOLON: ";" ;
LPAREN: "(" ;
RPAREN: ")" ;
COLON: ":" ;
DOTDOT: ".." ;
LBR: "[" ;
RBR: "]" ;
REF: "^" ;
LBRACE: "{" ;
RBRACE: "}" ;
NEQUAL: "#" ;
LESS: "<" ;
LEQ: "<=" ;
GREATER: ">" ;
GEQ: ">=" ;
PLUS: "+" ;
MINUS: "-" ;
SLASH: "/" ;
AMP: "&" ;
TILDE: "~" ;
STROKE: "|" ;
ASSIGN: ":=" ;
COMMENT: "(*" ( NEWLINE | (~('*'|'\n'|'\r')) | {LA(2)!=')'}?'*' )* "*)" {$setType(Token.SKIP);};
NEWLINE: ("\n" | "\r\n") {$setType(Token.SKIP);newline();};
SPACE: (" " | "\t") {$setType(Token.SKIP);};
NUMBER {boolean Hexed = false;}:
	DIGIT (DIGIT | HEX {Hexed = true;})* (
	('X'!|'x'!) {$setType(CHAR);}|
	('H'|'h') {$setType(INTEGER);}|
	{Hexed == false}? '.' (DIGIT)* (('E'|'e'|('D'|'d'){$setText('e');}) ('+'|'-')? (DIGIT)+)? {$setType(REAL);}|
	{Hexed == false}? {$setType(INTEGER);});
IDENT: LETTER (LETTER|DIGIT)*;
STRING: '\''!(~'\'')*'\''! | '\"'!(~'\"')*'\"'!;
protected DIGIT: '0'..'9';
protected HEX: 'A'..'F'|'a'..'f';
protected LETTER: 'A'..'Z'|'a'..'z'|'_';

class Oberon2Parser extends Parser;
options {
	//buildAST = true;
	defaultErrorHandler = false;
}

// Helpers
qualident returns [Token tok = null]: ((IDENT DOT)=> t1:IDENT DOT {tok = t1;}|) t2:IDENT 
	{
		if(tok == null)
			return t2;
		else {
			tok.setText(tok.getText() + "." + t2.getText());
		}
	};
integer returns [int l = 0] {Expression.Constant e = null;}: 
	e=constExpression {e = (Expression.Constant)Expression.TypeCast(Type.LongInt, e); l = (int)(long)(Long)e.evaluate();};


// Header
module {Identifier main = null; Statement mainBody = null;}: 
	"MODULE" mTok:IDENT SEMICOLON (importList)?
	{
		Module.addIdent(main = new Identifier(Type.VoidFunction, null), mTok);
		Module.enterFunction(main);
	}
	declarationSequence
    ("BEGIN" mainBody=statementSequence)? "END" mTok2:IDENT DOT 
    {
    	if(!mTok.getText().equals(mTok2.getText()))
	    	throw new Error(Error.COMPILEERROR, "Expected \"" + mTok.getText() + "\" instead of \"" + mTok2.getText() + "\"");
    	main.setValue(new FunctionValue(Module.leaveFunction(), mainBody));
    	Module.main = Statement.ProcedureCall(main.getNo(), new ArrayList<Expression>());
    };
importList: "IMPORT" t1:IDENT {Module.Import(t1.getText());} 
	(COMMA t2:IDENT {Module.Import(t2.getText());} )* SEMICOLON;

// Types
formalType returns [Type t = null]: 
	t=typeDef |
	t=formalArrayType;
type returns [Type t = null]: 
	t=typeDef|
	t=arrayType;
typeDef returns [Type tt = null] {Token t = null;}:
	t=qualident 
	{
		Identifier id = Module.idents.get(Module.getIdentNo(t));
		if(id.getType().typeId() != Type.TYPE)
			throw new Error(Error.COMPILEERROR, "Type expected");
		tt = (Type)id.getValue();
	};
nonArrayType returns [Type tt = null] {Token t = null;}:
	t=qualident 
	{
		Identifier id = Module.idents.get(Module.getIdentNo(t));
		if(id.getType().typeId() != Type.TYPE)
			throw new Error(Error.COMPILEERROR, "Type expected");
		if(((Type)id.getValue()).typeId() == Type.ARRAY)
			throw new Error(Error.COMPILEERROR, "2-dimensional arrays are not supported");
		tt = (Type)id.getValue();
	};
arrayType returns [Type tt = null] {int l = 0; Type t = null;}:
	"ARRAY" l=integer "OF" t=type {tt = Type.Array(l, t);};
formalArrayType returns [Type tt = null] {Type t = null;}:
	"ARRAY" "OF" t=nonArrayType {tt = Type.Array(0, t);};

// Declarations
declarationSequence: ("CONST" (constantDeclaration SEMICOLON)* |
    "TYPE" (typeDeclaration SEMICOLON)* | 
    "VAR" (variableDeclaration SEMICOLON)* |
    "PROCEDURE" (forwardDeclaration SEMICOLON | procedureDeclaration SEMICOLON))*;
constantDeclaration {Expression.Constant e = null;}:
	t:IDENT EQUAL e=constExpression
	{Module.addIdent(new Identifier(e.returnType(), e.evaluate()), t);};
typeDeclaration {Type tt = null;}: 
	t:IDENT EQUAL tt=type
	{Module.addIdent(new Identifier(Type.Type, tt), t);};
identList returns [ArrayList<Token> ts = new ArrayList<Token>()]: 
	t:IDENT {ts.add(t);} (COMMA t2:IDENT {ts.add(t2);})*;
variableDeclaration {ArrayList<Token> ts = null; Type t = null;}: 
	ts=identList COLON t=type {for(int i = 0; i < ts.size(); i++) Module.addIdent(new Identifier(t), ts.get(i));};
forwardDeclaration {Type tt = null;}:
	REF t:IDENT tt=formalParameters[false] {Module.addIdent(new Identifier(tt, null), t);};
procedureDeclaration{Type tt = null; Identifier funcIdent = null; Statement body = null;}:
	t:IDENT
	{
		Module.addIdent(funcIdent = new Identifier(Type.VoidFunction, null), t);
		Module.enterFunction(funcIdent.getNo());
	}
	tt=formalParameters[true] 
	{
		funcIdent.setType(tt);
	}
	SEMICOLON body=procedureBody t2:IDENT 
	{
		if(!t.getText().equals(t2.getText())) 
			throw new Error(Error.COMPILEERROR, "Expected \"" + t.getText() + "\" instead of \"" + t2.getText() + "\"");
		funcIdent.setValue(new FunctionValue(Module.leaveFunction(), body));
		Module.checkForwardDecl(funcIdent);
	};
procedureBody returns [Statement body = Statement.Nop()]:
	declarationSequence ("BEGIN" body=statementSequence)? "END";
formalParameters[boolean addVars] returns [Type t = null] 
	{
		Type retType = Type.Void;
		ArrayList<Type> paramTypes = new ArrayList<Type>();
		ArrayList<Boolean> varParam = new ArrayList<Boolean>();
	}:
	(LPAREN (fPSection[paramTypes, varParam, addVars] (SEMICOLON fPSection[paramTypes, varParam, addVars])*)? RPAREN)? 
	(COLON retType=nonArrayType)?
	{t = Type.Function(paramTypes.size(), paramTypes, varParam, retType);};
fPSection[ArrayList<Type> paramTypes, ArrayList<Boolean> varParam, boolean addVars]
	{
		boolean isVarParam = false;
		ArrayList<Token> toks = new ArrayList<Token>();
		Type type = null;
	}: 
	("VAR" {isVarParam = true;})? 
	t:IDENT {toks.add(t);} (COMMA t2:IDENT {toks.add(t2);})* COLON type=formalType
	{
		for(int i = 0; i < toks.size(); i++) {
			varParam.add(isVarParam);
			paramTypes.add(type);
			if(addVars)
				Module.addIdent(new Identifier(type), toks.get(i));
		}
	};

// Expressions
constExpression returns [Expression.Constant result = null] {Expression e = null;}: 
	e=expression
	{
		if(!(e instanceof Expression.Constant))
			throw new Error(Error.COMPILEERROR, "Constant expression expected");
		result = (Expression.Constant)e;
	};
expression returns [Expression e = null] {Expression e1 = null; int opCode = 0;}: 
	e=simpleExpression (opCode=relation e1=simpleExpression {e = Expression.Operation(opCode, e, e1);})?;
simpleExpression returns [Expression e = null] {int opCode = 0; Expression e1 = null;}: 
	((opCode=unaryAddOperator e=term {if(opCode == Expression.MINUS) e = Expression.Operation(opCode, e);}) | e=term)
	(opCode=addOperator e1=term {e = Expression.Operation(opCode, e, e1);})*;
term returns [Expression e = null] {int opCode = 0; Expression e1 = null;}: 
	e=factor (opCode=mulOperator e1=factor {e = Expression.Operation(opCode, e, e1);})*;
factor returns [Expression e = null] {Token t = null; Identifier v = null; Expression e1 = null;}: 
	t1:INTEGER 
	{
		long value = 0;
		if(t1.getText().endsWith("h") || t1.getText().endsWith("H"))
			value = Long.parseLong(t1.getText().substring(0, t1.getText().length() - 1), 16);
		else
			value = Long.parseLong(t1.getText());
		if(value >= Short.MIN_VALUE && value <= Short.MAX_VALUE)
			e = Expression.Constant(Type.ShortInt, (Short)(short)value);
		else if(value >= Integer.MIN_VALUE && value <= Integer.MAX_VALUE)
			e = Expression.Constant(Type.Integer, (Integer)(int)value);
		else
			e = Expression.Constant(Type.LongInt, (Long)value);
	}|
	
	t2:REAL {e = Expression.Constant(Type.Real, Float.parseFloat(t2.getText()));}|
	
	t3:CHAR 
	{
		long value = Long.parseLong(t3.getText(), 16);
		if(value < 0 || value > 255)
			throw new Error(Error.DOMAINERROR);
		e = Expression.Constant(Type.Char, (char)value);
	}|
	
	t4:STRING {e = Expression.Constant(Type.String, t4.getText());}|
	
	LPAREN e=expression RPAREN | 
	
	TILDE e=factor {e = Expression.Operation(Expression.NOT, e);} |
	
	t=qualident {v = Module.idents.get(Module.getIdentNo(t));} 
	({v.getType().typeId() == Type.FUNCTION}? e=actualParameters[v] | 
	(LBR e1=expression RBR)? {e = Expression.Variable(v.getNo(), e1);}) |
	
	"ABS" LPAREN e=expression RPAREN {e = Expression.Abs(e);} |

	"ASH" LPAREN e=expression COMMA e1=expression RPAREN {e = Expression.Ash(e, e1);} |
	
	"LSH" LPAREN e=expression COMMA e1=expression RPAREN {e = Expression.Lsh(e, e1);} |

	"CHR" LPAREN e=expression RPAREN {e = Expression.Chr(e);} |
	
	"ORD" LPAREN e=expression RPAREN {e = Expression.Ord(e);} |
	
	"ENTIER" LPAREN e=expression RPAREN {e = Expression.Entier(e);} |

	"EVEN" LPAREN e=expression RPAREN {e = Expression.Even(e);} |

	"ODD" LPAREN e=expression RPAREN {e = Expression.Odd(e);} |

	"LONG" LPAREN e=expression RPAREN {e = Expression.Long(e);} |

	"SHORT" LPAREN e=expression RPAREN {e = Expression.Short(e);} |
	
	"LEN" LPAREN e=expression RPAREN {e = Expression.Len(e);};
	
actualParameters[Identifier funcIdent] returns [Expression.FunctionCall e = null] 
	{ArrayList<Expression> args = new ArrayList<Expression>();}: 
	({funcIdent.getType().funcParamCount() > 0}? LPAREN args=actualParameters2[funcIdent, 0] RPAREN | LPAREN RPAREN |)
	{e = Expression.FunctionCall(funcIdent.getNo(), args);};
actualParameters2[Identifier funcIdent, int pos] returns [ArrayList<Expression> args = null] {Expression e = null;}:
	({funcIdent.getType().funcIsVarParam(pos)}?	e=designatorExpr | e=expression)
	({pos < funcIdent.getType().funcParamCount()}? COMMA args=actualParameters2[funcIdent, pos + 1] |)
	{
		if(args != null)
			args.add(0, e);
		else {
			args = new ArrayList<Expression>();
			args.add(e);
		}
	};
designatorExpr returns [Expression e = null] {Token t = null; Expression e1 = null;}:
	t=qualident (LBR e1=expression RBR)? 
	{
		if(Module.idents.get(Module.getIdentNo(t)).isConst())
			throw new Error(Error.COMPILEERROR, "Cannot pass constant as var parameter");
		e = Expression.Variable(Module.getIdentNo(t), e1);
	};

// Operators
relation returns [int opCode = 0]: 
	EQUAL   {opCode = Expression.EQUAL;}| 
	NEQUAL  {opCode = Expression.NOTEQUAL;}| 
	LESS    {opCode = Expression.LESS;}| 
	LEQ     {opCode = Expression.LESSEQUAL;}| 
	GREATER {opCode = Expression.GREATER;}| 
	GEQ     {opCode = Expression.GREATEREQUAL;};
unaryAddOperator returns [int opCode = 0]: 
	PLUS    {opCode = Expression.PLUS;}| 
	MINUS   {opCode = Expression.MINUS;};
addOperator returns [int opCode = 0]: 
	opCode=unaryAddOperator | 
	"OR"    {opCode = Expression.OR;};
mulOperator returns [int opCode = 0]:  
	STAR    {opCode = Expression.TIMES;}| 
	SLASH   {opCode = Expression.DIVIDE;}| 
	"DIV"   {opCode = Expression.DIV;}| 
	"MOD"   {opCode = Expression.MOD;}| 
	AMP     {opCode = Expression.AND;};

// Statements
statement returns [Statement s = Statement.Nop()]: 
	((qualident (ASSIGN | LBR))=> s=assignment | s=procedureCall |
	s=forStatement | s=ifStatement | s=whileStatement | s=repeatStatement | s=loopStatement | 
	s=exitStatement | s=returnStatement | s=defaultProc)?;
exitStatement returns [Statement s = Statement.Break()]:
	"EXIT";
returnStatement returns [Statement s = null] {Expression e = null;}:
	"RETURN" (e=expression {s = Statement.Return(e);} | {s = Statement.Return(Expression.Constant(Type.Void, null));});
assignment returns [Statement s = null] {Expression index = null, value = null; Token t = null; int varNo = 0;}:
	t=qualident {varNo = Module.getIdentNo(t);} 
	(LBR index=expression RBR)? ASSIGN value=expression {s = Statement.Assignment(varNo, index, value);};
procedureCall returns [Statement s = null] {Token t = null; Identifier funcIdent = null; Expression.FunctionCall call = null;}: 
	t=qualident {funcIdent = Module.idents.get(Module.getIdentNo(t));} call=actualParameters[funcIdent]
	{s = Statement.ProcedureCall(call);};
statementSequence returns [Statement.Sequence s = Statement.Sequence()] {Statement s1 = null, s2 = null;}:
	s1=statement {s.statements.add(s1);} (SEMICOLON s2=statement {s.statements.add(s2);})*;
ifStatement returns [Statement s = null]: 
	"IF" s=ifWithoutIf "END";
ifWithoutIf returns [Statement s = null] {Statement thenBlock = null, elseBlock = null; Expression expr = null;}: 
	expr=expression {Statement.If(expr, null, null);} "THEN" thenBlock=statementSequence
    ("ELSIF" elseBlock=ifWithoutIf | "ELSE" elseBlock=statementSequence)?
    {s = Statement.If(expr, thenBlock, elseBlock);};
whileStatement returns [Statement s = null] {Statement.Sequence seq = Statement.Sequence(); Expression expr = null;}:
	"WHILE" expr=expression {Statement.While(null, expr);} "DO" seq=statementSequence "END"
	{s = Statement.While(seq, expr);};
repeatStatement returns [Statement s = null] {Statement.Sequence seq = Statement.Sequence(); Expression expr = null;}:
	"REPEAT" seq=statementSequence "UNTIL" expr=expression
	{s = Statement.Repeat(seq, expr);};
loopStatement returns [Statement s = null] {Statement block = null;}: 
	"LOOP" block=statementSequence "END"
	{s = Statement.Loop(block);};
forStatement returns [Statement s = null] 
	{
		Token t = null; 
		int varNo = 0; 
		Expression from = null, to = null, by = Expression.Constant(Type.ShortInt, (Short)(short)1);
		Statement block = null;
	}: 
	"FOR" t=qualident {varNo = Module.getIdentNo(t);} 
	ASSIGN from=expression "TO" to=expression ("BY" by=constExpression)? {Statement.For(varNo, from, to, by, null);}
	"DO" block=statementSequence "END"
	{s = Statement.For(varNo, from, to, by, block);};
defaultProc returns [Statement s = null] {Token t = null; int varNo = 0, code = 0; Expression index = null, expr = null;}:
	"INC" LPAREN t=qualident {varNo = Module.getIdentNo(t);} (LBR index=expression RBR)? (COMMA expr=expression)? RPAREN {s = Statement.Inc(varNo, index, expr);}|
	"DEC" LPAREN t=qualident {varNo = Module.getIdentNo(t);} (LBR index=expression RBR)? (COMMA expr=expression)? RPAREN {s = Statement.Dec(varNo, index, expr);}|
	"HALT" LPAREN code=integer RPAREN {s = Statement.Halt(code);};


