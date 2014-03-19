header {
package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;
import antlr.TokenStreamRecognitionException;
}

// ================================================================================= \\
//                                       LEXER
// ================================================================================= \\
class Oberon2Lexer extends Lexer;
options {
	//testLiterals = false;
	k = 2;
    defaultErrorHandler = false;
    charVocabulary = '\u0000'..'\uFFFE';
}
tokens {
  INC;
  DEC;
  BREAK;
  CONTINUE;
  HALT;
  ODD;
  EVEN;
  ABS;
  TRUE;
  FALSE;
}

COMMENT : "(*" (NEWLINE | ('\"' (~'\"')* '\"') | ('\'' (~'\'')* '\'') | COMMENT | 
	(~('*'|'\r'|'\n'|'('|'\''|'\"')) | '*'(~')') | '('(~'*'))* "*)" {$setType(Token.SKIP);};
NEWLINE : ("\r\n" | '\n') {$setType(Token.SKIP);newline();};
WHITESPACE: (' ' | '\t') {$setType(Token.SKIP);};
STAR : "*";
DOT : "."; 
EQUAL : "=" ;
COMMA : "," ;
SEMICOLON : ";" ;
LPAREN : "(" ;
RPAREN : ")" ;
ASSIGN : ":=" ;
COLON : ":" ;
DOTDOT : ".." ;
LBR : "[" ;
RBR : "]" ;
REF : "^" ;
LBRACE : "{" ;
RBRACE : "}" ;
NEQUAL : "#" ;
LEQ : "<=" ;
LESS : "<" ;
GEQ : ">=" ;
GREATER : ">" ;
PLUS : "+" ;
MINUS : "-" ;
SLASH : "/" ;
AMP : "&" ;
TILDE : "~" ;
STROKE : "|" ;
NUMERIC : ((DIGIT)+ '.')=> FLOAT {$setType(FLOAT);} | 
          (CHAR)=> CHAR {$setType(CHAR);} |
          INT {$setType(INT);};
protected INT : ((DIGIT)+ HEXLETTER)=> DIGIT (HEXDIGIT)* ('H' | 'h') | (DIGIT)+;
protected FLOAT : (DIGIT)+ '.' (DIGIT)* (('e' | 'E' | 'd' | 'D') ('+' | '-')? (DIGIT)+)?;
protected CHAR : DIGIT (HEXDIGIT)* ('X' | 'x');
STRING : '\"' ((~'\"') | ("\"\""))* '\"' | '\'' ((~'\'') | ("\'\'"))* '\'';
IDENT /*options { testLiterals = true; }*/ : LETTER ( LETTER | DIGIT) * 
		{
			if(((String)$getText).equals("BREAK"))
				$setType(BREAK);
			else if(((String)$getText).equals("CONTINUE"))
				$setType(CONTINUE);
			else if(((String)$getText).equals("INC"))
				$setType(INC);
			else if(((String)$getText).equals("DEC"))
				$setType(DEC);
			else if(((String)$getText).equals("HALT"))
				$setType(HALT);
			else if(((String)$getText).equals("ODD"))
				$setType(ODD);
			else if(((String)$getText).equals("EVEN"))
				$setType(EVEN);
			else if(((String)$getText).equals("ABS"))
				$setType(ABS);
			else if(((String)$getText).equals("TRUE"))
				$setType(TRUE);
			else if(((String)$getText).equals("FALSE"))
				$setType(FALSE);
		};
protected DIGIT : '0' .. '9' ;
protected HEXLETTER : 'a' .. 'f' | 'A' .. 'F';
protected HEXDIGIT : DIGIT | HEXLETTER;
protected LETTER : 'A' .. 'Z' | 'a' .. 'z' | '_';

// ================================================================================= \\
//                                       PARSER
// ================================================================================= \\
class Oberon2Parser extends Parser;
options {
	// buildAST = true;
	k = 2;
    defaultErrorHandler = false;
}
{
	protected int errLine;
	protected int errCol;
	protected Token errToken;
	protected Context context;
	public Context Parse(String fileName) throws ANTLRException {
		context = new Context();
		context.fileName = fileName;
		try {
			module();
		} catch (RuntimeError e) {
			if(Oberon.Debug)
				e.printStackTrace(System.err);
			if(errToken == null)
				errToken = LT(0);
			if(errLine == 0) {
				errLine = errToken.getLine();
				errCol = errToken.getColumn();
			}
			throw new RuntimeError(e, fileName + ":" + errLine + ":" + errCol + ":" + e.getMessage());
		} catch (TokenStreamRecognitionException e) {
			if(Oberon.Debug)
				e.printStackTrace(System.err);
			throw new RuntimeError(new RuntimeError(Error.PARSEERROR), fileName + ":" + e.recog.line + ":" + e.recog.column + ":" + e.getMessage());
		} catch (Exception e) {
			if(Oberon.Debug)
				e.printStackTrace(System.err);
			if(errToken == null)
				errToken = LT(0);
			if(errLine == 0 && errToken != null) {
				errLine = errToken.getLine();
				errCol = errToken.getColumn();
			}
			else if(errToken == null) {
				errLine = 1;
				errCol = 1;
			}
			throw new RuntimeError(new RuntimeError(Error.PARSEERROR), fileName + ":" + errLine + ":" + errCol + ":" + e.getMessage());
		}
		return context;
	}
}

// Idents & likes
identdef returns [Token token = null]: t:IDENT {token = t;}/*(STAR)?*/;

qualident returns [Token token = null] {boolean isPrefixed = false;}:
	(t1:IDENT DOT 
		{
			isPrefixed = true;
			token = t1;
		}
	)? t2:IDENT 
		{
			if(isPrefixed)
				token.setText(token.getText() + "." + t2.getText());
			else
				token = t2;
		};

identList returns [ArrayList<Token> identTokens = new ArrayList<Token>()] {Token t;}:
	t=identdef {identTokens.add(t);} (COMMA t=identdef {identTokens.add(t);})*;
	
typeid returns [Type type = null] {Token t = null;}: 
	t=qualident 
		{
			type = context.getType(t.getText());
		};

// Module Header
module
		{
			Variable main = null;
		}:
	"MODULE" t1:IDENT SEMICOLON
		{
			main = new Variable(t1.getText(), Type.SimpleProcType, new CodeBlock(), 0, 0, true);
			context.AddVariable(main);
			context.mainId = main.id;
			//context.AddVariable(new Variable(t1, BuiltInType.MODULE, null, true));
		}
	(importList)? 
		{
			context.EnterBlock(main);
		}
	declarationSequence
	("BEGIN" statementSequence)? endToken:"END" t2:IDENT DOT 
		{
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
			context.codeBlock().statements.add(AsmStatement.Exit(endToken));
			if(!t1.getText().equals(t2.getText()))
				throw new SemanticException("Expected \"" + t1.getText() + "\", got \"" + t2.getText() + "\"");
			context.LeaveBlock();
		};

importList:
	"IMPORT" obimport (COMMA obimport)* SEMICOLON ;

obimport {boolean importedAs = false;}:
	t1:IDENT 
	(ASSIGN t2:IDENT 
		{
			importedAs = true;
			context.ImportModule(t2, t1);
		}
	)?
		{
			if(!importedAs)
				context.ImportModule(t1);
		};

// ================================================================================= \\
// Decls
// ================================================================================= \\
// Decls
declarationSequence: 
	("CONST" (constantDeclaration SEMICOLON)* |
	"TYPE" (typeDeclaration SEMICOLON)* |
	"VAR" (variableDeclaration SEMICOLON)* | 
	(procedureDeclaration SEMICOLON | forwardDeclaration SEMICOLON))*;

// ProcDecl
forwardDeclaration 
		{
			Pair<Type, ArrayList<Token>> procType = new Pair<Type, ArrayList<Token>>(Type.SimpleProcType, null);
		}:
	"PROCEDURE" REF t:IDENT /*(STAR)?*/ procType=formalParameters
		{
			context.AddVariable(new Variable(t, procType.first, null, true));
		};
	
procedureDeclaration 
		{
			Token nameToken = null; 
			Pair<Type, ArrayList<Token>> procType = new Pair<Type, ArrayList<Token>>(Type.SimpleProcType, null); 
			CodeBlock block = null;
			Variable procVar = null;
		}:
	"PROCEDURE" /*(STAR)?*/ nameToken=identdef 
	procType=formalParameters SEMICOLON 
		{
			procVar = new Variable(nameToken, procType.first, new CodeBlock(), true);
			context.AddVariable(procVar);
			context.EnterBlock(procVar);
			if(procType.second != null)
				for(int i = 0; i < procType.second.size(); i++)
					context.AddVariable(new Variable(procType.second.get(i), procType.first.paramTypes().get(i), null, false));
		}
	declarationSequence ("BEGIN" statementSequence)? endToken:"END" t:IDENT
		{
			if(!nameToken.getText().equals(t.getText()))
				throw new SemanticException("Expected \"" + nameToken.getText() + "\", got \"" + t.getText() + "\"");
			context.codeBlock().statements.add(AsmStatement.Push(TypedValue.VoidTypedValue));
			context.codeBlock().statements.add(AsmStatement.Ret(endToken));
			context.LeaveBlock();
		};

formalParameters returns [Pair<Type, ArrayList<Token>> result = null]
		{
			Triad<ArrayList<Type>, ArrayList<Boolean>, ArrayList<Token>> tmpParams = null;
			ArrayList<Type> paramTypes = new ArrayList<Type>();
			ArrayList<Boolean> isRefParam = new ArrayList<Boolean>();
			ArrayList<Token> tokenList = new ArrayList<Token>();
			Type returnType = Type.VoidType;
		}:
	(LPAREN (tmpParams=fPSection
		{
			paramTypes.addAll(tmpParams.first);
			isRefParam.addAll(tmpParams.second);
			tokenList.addAll(tmpParams.third);
		}
	(SEMICOLON tmpParams=fPSection
		{
			paramTypes.addAll(tmpParams.first);
			isRefParam.addAll(tmpParams.second);
			tokenList.addAll(tmpParams.third);
		}
	)*)? RPAREN)? (COLON returnType=typeid)?
	{
		if(returnType.type == BuiltInType.ARRAY)
			throw new SemanticException("Cannot return array from a function");
		result = new Pair(new Type(BuiltInType.PROCEDURE, paramTypes, isRefParam, returnType), tokenList);
	};

fPSection returns [Triad<ArrayList<Type>, ArrayList<Boolean>, ArrayList<Token>> result = null] 
		{
			ArrayList<Token> paramTokens = new ArrayList<Token>();
			boolean isRefParam = false;
			Type type = null;
		}:
	("VAR" {isRefParam = true;})? 
	t1:IDENT {paramTokens.add(t1);} (COMMA t2:IDENT {paramTokens.add(t2);})* COLON type=formalType
		{
			ArrayList<Type> typeList = new ArrayList<Type>();
			ArrayList<Boolean> boolList = new ArrayList<Boolean>();
			for(int i = 0; i < paramTokens.size(); i++)
			{
				boolList.add(isRefParam);
				typeList.add(type);
			}
			result = new Triad<ArrayList<Type>, ArrayList<Boolean>, ArrayList<Token>>(typeList, boolList, paramTokens);
		};

formalType returns [Type type = null] {int arrayCount = 0;}:  
	("ARRAY" "OF" {arrayCount++;})* 
	(type=typeid/* | procedureType*/)
		{
			while(arrayCount-- > 0)
				type = new Type(BuiltInType.ARRAY, type, 0);
		};

// Constant Decl
constantDeclaration {TypedValue value = null; Token t = null;}: 
	t=identdef EQUAL value=constExpression
		{
			context.AddVariable(new Variable(t, value, true));
		};

// Type Decl
typeDeclaration {Token nameToken = null; Type type = null;}: 
	nameToken=identdef EQUAL type=typedef
		{
			type.name = nameToken.getText();
			context.AddType(type);
		};

typedef returns [Type type = null] {Token nameToken = null;}: 
	nameToken=qualident {type = context.getType(nameToken.getText());} | 
	type=arrayType /*| recordType | pointerType | procedureType*/;

arrayType returns [Type type = null] 
		{
			ArrayList<Integer> lengths = new ArrayList<Integer>(); 
			int currentLenght = 0;
		}: 
	"ARRAY" currentLenght=length {lengths.add(currentLenght);} 
	(COMMA currentLenght=length {lengths.add(currentLenght);})* "OF" type=typedef
		{
			for(int i = lengths.size() - 1; i >= 0; i--)
				type = new Type(BuiltInType.ARRAY, type, lengths.get(i));
		};

length returns [int result = 0] {TypedValue value = null;}:
	value=constExpression 
		{
			result = (Integer)value.ConvertTo(Type.IntegerType).value;
			if(result <= 0)
				throw new SemanticException("Cannot define array of non-positive length");
		};
	
//recordType  :  "RECORD" (LPAREN baseType RPAREN)? fieldListSequence "END";
//fieldListSequence  :  fieldList (SEMICOLON fieldList)*;
//fieldList  :  (identList COLON type)?;
//pointerType  :  "POINTER" "TO" type;
//procedureType : "PROCEDURE" (formalParameters)?;
//baseType:  qualident;

// Var Decl
variableDeclaration {ArrayList<Token> varTokens = null; Type type = null;}:
	varTokens=identList COLON type=typedef
		{
			for(int i = 0; i < varTokens.size(); i++)
				context.AddVariable(new Variable(varTokens.get(i), type, null, false));
		};

// ================================================================================= \\
// Statements
// ================================================================================= \\
// Statements
statementSequence: statement (SEMICOLON statement)*;
statement:
	(simpleStatement | forStatement | ifStatement | caseStatement | whileStatement | repeatStatement | 
	loopStatement | /*withStatement |*/ returnStatement | breakStatement | continueStatement | builtInProc)?;

builtInProc
		{
			Token beginToken = null;
			Type lValueType = null;
			ExpressionResult r = new ExpressionResult(false, new TypedValue(Type.ShortIntType));
		}: 
	incToken:INC LPAREN lValueType=varParam 
		{
			context.codeBlock().statements.add(AsmStatement.Dup(0, incToken));
		}
	(COMMA r=nonConstExpression | 
		{
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.ShortIntType, (Short)(short)1)));
		}) RPAREN
		{
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.PLUS, incToken));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.ASSIGN, incToken));
			context.codeBlock().statements.add(AsmStatement.Pop(incToken));
			Operation.ASSIGN.returnType(lValueType, Operation.PLUS.returnType(lValueType, r.value.type));
		}| 
	decToken:DEC LPAREN lValueType=varParam 
		{
			context.codeBlock().statements.add(AsmStatement.Dup(0, decToken));
		}
	(COMMA r=nonConstExpression | 
		{
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.ShortIntType, (Short)(short)1)));
		}) RPAREN
		{
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.MINUS, decToken));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.ASSIGN, decToken));
			context.codeBlock().statements.add(AsmStatement.Pop(decToken));
			Operation.ASSIGN.returnType(lValueType, Operation.PLUS.returnType(lValueType, r.value.type));
		}| 
	haltToken:HALT (LPAREN r=nonConstExpression RPAREN 
		{
			if(!r.value.type.convertableTo(Type.LongIntType))
				throw new SemanticException("Cannot convert \"" + r.value.type + "\" to \"" + Type.LongIntType + "\"");
		}| 
		{
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
		}) 
		{
			context.codeBlock().statements.add(AsmStatement.Exit(haltToken));
		}|
		{
			beginToken = LT(0);
		}
	r=builtInExpr
		{
			context.codeBlock().statements.add(AsmStatement.Pop(beginToken));
		}; 

breakStatement: 
	(breakToken:BREAK | exitToken:"EXIT")
		{
			if(breakToken == null)
				breakToken = exitToken;
			if(context.CurrentBlockJmpToEndPosition() != null)
				context.codeBlock().statements.add(AsmStatement.Jmp(context.CurrentBlockJmpToEndPosition()));
			else
				throw new SemanticException("No cycle to leave");
		};

continueStatement:  
	contToken:CONTINUE
		{
			if(context.CurrentBlockJmpToEndPosition() != null)
				context.codeBlock().statements.add(AsmStatement.Jmp(context.CurrentBlockJmpToEndPosition() - 1));
			else
				throw new SemanticException("No cycle to continue");
		};
		
simpleStatement: (qualident (ASSIGN | LBR))=> assignment | procedureCall;

returnStatement
		{
			boolean returned = false;
			ExpressionResult r = new ExpressionResult(false, new TypedValue(Type.VoidType));
		}:
	retToken:"RETURN" (r=nonConstExpression {returned = true;})?
		{
			Variable f = context.variables.get(context.getVariableId(context.blockName()));
			if(!returned)
				context.codeBlock().statements.add(AsmStatement.Push(TypedValue.VoidTypedValue));
			context.codeBlock().statements.add(AsmStatement.Ret(retToken));
			errToken = retToken;
			if(returned)
				Operation.ASSIGN.returnType(f.type.returnType(), r.value.type);
			else if(f.type.returnType().type != BuiltInType.VOID) {
				throw new SemanticException("Cannot return in procedure with no return value");
			}
			errToken = null;
		};

// Proc Call
procedureCall
		{
			Token procToken = null;
			Type param0Type = null;
		}:
	procToken=qualident param0Type=actualParameters[procToken]
		{
			Variable proc = context.variables.get(context.getVariableId(procToken.getText()));
    		if(proc.type.type != BuiltInType.PROCEDURE)
    			throw new SemanticException("\"" + procToken.getText() + "\" is not a procedure");
    		context.codeBlock().statements.add(AsmStatement.Call(proc.id, procToken));
    		context.codeBlock().statements.add(AsmStatement.Pop(procToken)); // pop the result we don't need
		};

// Assignment
assignment {Type lValueType = null; ExpressionResult r = null;}: 
	lValueType=lvalue asgnToken:ASSIGN r=nonConstExpression
		{
    		context.codeBlock().statements.add(AsmStatement.BinOp(Operation.ASSIGN, asgnToken));
    		context.codeBlock().statements.add(AsmStatement.Pop(asgnToken)); // Pop Void from Assign
    		errToken = asgnToken;
    		Operation.ASSIGN.returnType(lValueType, r.value.type);
    		errToken = null;
		};
lvalue returns [Type type = null]
		{
			Token t = null;
			ExpressionResult r = null;
			Variable var = null;
		}:
	t=qualident 
		{
			var = context.variables.get(context.getVariableId(t.getText()));
			if(var.isConst)
				throw new SemanticException("Can't assign to a constant");
			context.codeBlock().statements.add(AsmStatement.PushVar(var.id, t));
			r = new ExpressionResult(false, new TypedValue(var.type));
		}
	r=arrayIndex[var, r]
		{
			type = r.value.type;
		};

arrayIndex[Variable v, ExpressionResult defResult] returns [ExpressionResult result = null]
	{
		ExpressionResult r = null;
	}:
	{
		result = defResult;
	}
	(lbrToken:LBR r=nonConstExpression 
		{
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.INDEX, lbrToken));
			result.value.type = Operation.INDEX.returnType(result.value.type, r.value.type);
		}
	(COMMA r=nonConstExpression
		{
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.INDEX, lbrToken));
			result.value.type = Operation.INDEX.returnType(result.value.type, r.value.type);
		}
	)* RBR)*;
	

// Designator
//designator: qualident (/*DOT IDENT |*/ LBR expression (COMMA expression)* RBR /*| LPAREN qualident RPAREN | REF*/ )*;

// if 
/* Here goes IF ASM structure
	expr
	JMPNZ - to "then" section
	NOT
	JMPNZ - to "else" section
	JMP - to the end
	// then block 
	JMP - go JMP leading to the end
	// else block 
	-> end is here 
*/
ifStatement: "IF" ifFromExpr "END";
ifFromExpr
		{
			int jmpToEndPosition = 0;
			Token firstToken = LT(1);
		}:
	boolExpression "THEN" 
		{
			jmpToEndPosition = context.codeBlock().statements.size() + 2;
			context.codeBlock().statements.add(AsmStatement.JmpNZ(jmpToEndPosition + 1, firstToken));
			context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition)); // Skip else for now
			context.codeBlock().statements.add(AsmStatement.Jmp(0));
		}
	statementSequence 
		{
			context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition));
		}
	(	
		{
			context.codeBlock().statements.set(jmpToEndPosition - 1, AsmStatement.Jmp(context.codeBlock().statements.size()));
		}
	("ELSIF" ifFromExpr | "ELSE" statementSequence))?
		{
			context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
		};

// Case
caseStatement  :  "CASE"/* expression "OF" obcase (STROKE obcase)*
        ("ELSE" statementSequence)? "END"*/;
/*obcase  :  (caseLabelList COLON statementSequence)?;
caseLabelList  :  caseLabels (COMMA caseLabels)*;
caseLabels  :  expression (DOTDOT expression)?;*/

// While
/* Here goes WHILE ASM structure:
	expr
	JMPNZ - to "body" section
	JMP+2
	JMP - to "expr" section
	JMP - to the end
	// body
	-> end is here
	so setting jmpToEndPosition, we get that 
	BREAK is JMP(jmpToEndPosition) and CONTINUE is JMP(jmpToEndPosition - 1)
*/
whileStatement
		{
			int exprPosition = context.codeBlock().statements.size();
			int jmpToEndPosition = 0;
			Token firstToken = null;
		}: 
	"WHILE" 
		{
			firstToken = LT(1);
		}
	boolExpression doToken:"DO" 
		{
			jmpToEndPosition = context.codeBlock().statements.size() + 3;
			context.codeBlock().statements.add(AsmStatement.JmpNZ(jmpToEndPosition + 1, firstToken));
			context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition));
			context.codeBlock().statements.add(AsmStatement.Jmp(exprPosition));
			context.codeBlock().statements.add(AsmStatement.Jmp(0));
			context.EnterBeginEndBlock(jmpToEndPosition);
		}
	statementSequence "END"
		{
			context.codeBlock().statements.add(AsmStatement.Jmp(exprPosition));
			context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
			context.LeaveBeginEndBlock();
		};

// Repeat
/* Here goes REPEAT ASM structure:
	JMP+2
	JMP - to the end
	// body
	// expr
	JMPNZ - to the end
	JMP to begin
	-> end is here
	so setting jmpToEndPosition, we get that 
	BREAK is JMP(jmpToEndPosition) and CONTINUE is JMP(jmpToEndPosition - 1)
*/
repeatStatement
		{
			int jmpToEndPosition = 0;
			Token firstToken = null;
		}: 
	"REPEAT" 
		{
			jmpToEndPosition = context.codeBlock().statements.size() + 1;
			context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition + 1));
			context.codeBlock().statements.add(AsmStatement.Jmp(0));
			context.EnterBeginEndBlock(jmpToEndPosition);
		}
	statementSequence "UNTIL" 
		{
			firstToken = LT(1);
		}
	boolExpression
		{
			context.codeBlock().statements.add(AsmStatement.JmpNZ(jmpToEndPosition, firstToken));
			context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition - 1));
			context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
			context.LeaveBeginEndBlock();
		};

// Loop
/* Loop is really simple:
	JMP+2
	JMP - to the end
	// body
	JMP to begin
	-> end is here
*/
loopStatement		
		{
			int jmpToEndPosition = 0;
		}: 
	"LOOP" 
		{
			jmpToEndPosition = context.codeBlock().statements.size() + 1;
			context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition + 1));
			context.codeBlock().statements.add(AsmStatement.Jmp(0));
			context.EnterBeginEndBlock(jmpToEndPosition);
		}
	statementSequence "END"
		{
			context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition - 1));
			context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
			context.LeaveBeginEndBlock();
		};

// With
// withStatement  :  "WITH" qualident COLON qualident "DO" statementSequence "END" ;

// For
/* For is not trivial...
	
*/
forStatement
		{
			int jmpToEndPosition = 0;
			int jmpNZToIncPosition = 0;
		}:
	forToken:"FOR" 
		{
			context.EnterBeginEndBlock(context.codeBlock().statements.size() + 5);
			jmpToEndPosition = context.codeBlock().statements.size() + 9;
			context.codeBlock().statements.add(AsmStatement.Jmp(context.codeBlock().statements.size() + 12)); // to Begin
			// Break
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
			context.codeBlock().statements.add(AsmStatement.Jmp(context.codeBlock().statements.size() + 2));
			context.codeBlock().statements.add(AsmStatement.Jmp(context.codeBlock().statements.size() + 6));
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
			context.codeBlock().statements.add(AsmStatement.Jmp(0));  // To end
			// Continue
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.BooleanType, (Boolean)true)));
			context.codeBlock().statements.add(AsmStatement.Jmp(context.codeBlock().statements.size() + 2));
			// Begin
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.BooleanType, (Boolean)false)));
		}
	lvalue
		{
			context.codeBlock().statements.add(AsmStatement.Dup(1, forToken));
			jmpNZToIncPosition = context.codeBlock().statements.size();
			context.codeBlock().statements.add(AsmStatement.JmpNZ(0, forToken)); // To INC -- fixed later
			// Init
			context.codeBlock().statements.add(AsmStatement.Dup(0, forToken));
		}
	ASSIGN nonConstExpression "TO"
		{
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.ASSIGN, forToken));
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
		}
	nonConstExpression ("BY" nonConstExpression | 
		{
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.ShortIntType, (Short)(short)1)));
		})
		{
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.BooleanType, (Boolean)true)));
			context.codeBlock().statements.add(AsmStatement.Dup(3, forToken));
			context.codeBlock().statements.add(AsmStatement.Dup(0, forToken));
			context.codeBlock().statements.add(AsmStatement.Jmp(context.codeBlock().statements.size() + 8)); // To CMP
			
			// INC
			context.codeBlock().statements.set(jmpNZToIncPosition, AsmStatement.JmpNZ(context.codeBlock().statements.size(), forToken));
			context.codeBlock().statements.add(AsmStatement.Dup(0, forToken));
			context.codeBlock().statements.add(AsmStatement.Dup(3, forToken));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.PLUS, forToken));
			context.codeBlock().statements.add(AsmStatement.Dup(1, forToken));
			context.codeBlock().statements.add(AsmStatement.Dup(1, forToken));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.ASSIGN, forToken));
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
			
			// CMP
			context.codeBlock().statements.add(AsmStatement.Dup(3, forToken));
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.GREATER, forToken));
			context.codeBlock().statements.add(AsmStatement.JmpNZ(context.codeBlock().statements.size() + 6, forToken)); // To GREATER
			context.codeBlock().statements.add(AsmStatement.Dup(3, forToken));
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.LESS, forToken));
			context.codeBlock().statements.add(AsmStatement.JmpNZ(context.codeBlock().statements.size() + 6, forToken)); // To LESS
			context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition - 8)); // To END
			// GREATER
			context.codeBlock().statements.add(AsmStatement.Dup(4, forToken));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.GREATER, forToken));
			context.codeBlock().statements.add(AsmStatement.JmpNZ(jmpToEndPosition - 8, forToken)); // To END
			context.codeBlock().statements.add(AsmStatement.Jmp(context.codeBlock().statements.size() + 4)); // To BlockStart
			// LESS
			context.codeBlock().statements.add(AsmStatement.Dup(4, forToken));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.LESS, forToken));
			context.codeBlock().statements.add(AsmStatement.JmpNZ(jmpToEndPosition - 8, forToken)); // To END
			// Pop shit
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
			context.codeBlock().statements.add(AsmStatement.Pop(forToken));
		}
	"DO" statementSequence "END" 
		{
			context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition + 1));
			context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
			context.LeaveBeginEndBlock();
		};

// ================================================================================= \\
// Exprs
// ================================================================================= \\
// Exprs
//expList: expression[false] (COMMA expression[false])*;

boolExpression {ExpressionResult r = null;}:
	r=nonConstExpression
		{
			if(r.value.type.type != BuiltInType.BOOLEAN)
				throw new SemanticException("Boolean expression expected");
		};

nonConstExpression returns [ExpressionResult result = null]:
	result=expression
		{
			if(result.isConst) {
				context.codeBlock().statements.add(AsmStatement.Push(result.value));
				result.isConst = false;
			}
		};

constExpression returns [TypedValue result = null]
		{
			ExpressionResult expr = null;
		}:
	expr=expression
		{
			if(!expr.isConst)
				throw new SemanticException("Constant expected");
			result = expr.value;
		};

expression returns [ExpressionResult result = null]
		{
			Operation op = null;
			ExpressionResult result2 = null;
			Token opToken = null;
		}:
	result=simpleExpression
		{
			if(result.isConst)
				context.codeBlock().statements.add(AsmStatement.Push(result.value));
		}
	(
		{
			opToken = LT(1);
		}
	op=relation result2=simpleExpression 
		{
			if(result.isConst && result2.isConst)
			{
				context.codeBlock().removeLastStatement();
				result.value = op.Perform(result.value, result2.value);
				context.codeBlock().statements.add(AsmStatement.Push(result.value));
			}
			if(!result2.isConst || !result.isConst)
			{
				result.isConst = false;
				if(result2.isConst)
					context.codeBlock().statements.add(AsmStatement.Push(result2.value));
				context.codeBlock().statements.add(AsmStatement.BinOp(op, opToken));
				result.value = new TypedValue(op.returnType(result.value.type, result2.value.type));
			}
		}
	)?	{
			if(result.isConst)
				context.codeBlock().removeLastStatement();
		};

relation returns [Operation op = null]: 
	EQUAL   {op = Operation.EQUAL;}| 
	NEQUAL  {op = Operation.NOTEQUAL;}| 
	LESS    {op = Operation.LESS;}| 
	LEQ     {op = Operation.LESSOREQUAL;}| 
	GREATER {op = Operation.GREATER;}| 
	GEQ     {op = Operation.GREATEROREQUAL;} /*| "IN" | "IS"*/;

simpleExpression returns [ExpressionResult result = null]
		{
			Operation op = null;
			Operation unop = null;
			ExpressionResult result2 = null;
			Token opToken = LT(1);
		}:
	(unop=plusMinus)? result=term 
		{
			if(result.isConst)
			{
				if(unop != null && unop != Operation.PLUS)
					result.value = unop.Perform(result.value);
				context.codeBlock().statements.add(AsmStatement.Push(result.value));
			}
			else if(unop != null && unop != Operation.PLUS)
				context.codeBlock().statements.add(AsmStatement.UnOp(unop, opToken));
		}
	(
		{
			opToken = LT(1);
		}
	op=addOperator result2=term
		{
			if(result.isConst && result2.isConst)
			{
				context.codeBlock().removeLastStatement();
				result.value = op.Perform(result.value, result2.value);
				context.codeBlock().statements.add(AsmStatement.Push(result.value));
			}
			if(!result2.isConst || !result.isConst)
			{
				result.isConst = false;
				if(result2.isConst)
					context.codeBlock().statements.add(AsmStatement.Push(result2.value));
				context.codeBlock().statements.add(AsmStatement.BinOp(op, opToken));
				result.value = new TypedValue(op.returnType(result.value.type, result2.value.type));
			}
		}
	)*  {
			if(result.isConst)
				context.codeBlock().removeLastStatement();
		};

addOperator returns [Operation op = null]: 
	op=plusMinus | 
	"OR"  {op = Operation.OR;};

plusMinus returns [Operation op = null]: 
	PLUS  {op = Operation.PLUS;}| 
	MINUS {op = Operation.MINUS;};

term returns [ExpressionResult result = null]
		{
			Operation op = null;
			ExpressionResult result2 = null;
			Token opToken = null;
		}:
	result=factor 
		{
			if(result.isConst)
				context.codeBlock().statements.add(AsmStatement.Push(result.value));
		}
	(
		{
			opToken = LT(1);
		}
	op=mulOperator result2=factor
		{
			if(result.isConst && result2.isConst)
			{
				context.codeBlock().removeLastStatement();
				result.value = op.Perform(result.value, result2.value);
				context.codeBlock().statements.add(AsmStatement.Push(result.value));
			}
			if(!result2.isConst || !result.isConst)
			{
				result.isConst = false;
				if(result2.isConst)
					context.codeBlock().statements.add(AsmStatement.Push(result2.value));
				context.codeBlock().statements.add(AsmStatement.BinOp(op, opToken));
				result.value = new TypedValue(op.returnType(result.value.type, result2.value.type));
			}
		}
	)*  {
			if(result.isConst)
				context.codeBlock().removeLastStatement();
		};

mulOperator returns [Operation op = null]: 
	STAR  {op = Operation.TIMES;}| 
	SLASH {op = Operation.DIVIDE;}| 
	"DIV" {op = Operation.DIV;}| 
	"MOD" {op = Operation.MOD;}| 
	AMP   {op = Operation.AND;};

factor returns [ExpressionResult result = null]
		{
			Token procToken = null;
			boolean isProc = false;
			Variable v = null;
			Type param0Type = null;
		}:
	t1:INT 
		{
			String text = t1.getText();
			Object value = null;
			if(text.endsWith("h") || text.endsWith("H"))
				value = Long.parseLong(text.substring(1, text.length() - 1), 16);
			else
				value = Long.parseLong(text);
			Type type = Type.LongIntType;
			if((Long)value > Integer.MIN_VALUE && (Long)value < Integer.MAX_VALUE)
			{
				value = (Integer)(int)(long)(Long)value;
				type = Type.IntegerType;
				if((Integer)value > Short.MIN_VALUE && (Integer)value < Short.MAX_VALUE)
				{
					value = (Short)(short)(int)(Integer)value;
					type = Type.ShortIntType;
				}
			}
			result = new ExpressionResult(true, new TypedValue(type, value));
		}| 
	t2:FLOAT 
		{
			result = new ExpressionResult(true, new TypedValue(Type.RealType, Float.parseFloat(t2.getText().replace('d', 'e').replace('D','e'))));
		}| 
	t3:CHAR
		{
			result = new ExpressionResult(true, new TypedValue(Type.CharType, (char)Long.parseLong(t3.getText().replace("x", "").replace("X",""), 16)));
		}| 
	t4:STRING
		{
			String tmp = t4.getText().substring(1, t4.getText().length() - 1);
			if(t4.getText().charAt(0) == '\"')
				tmp = tmp.replace("\"\"","\0").replace("\0","\"");
			else
				tmp = tmp.replace("\'\'","\0").replace("\0","\'");
			result = new ExpressionResult(true, new TypedValue(Type.StringType, tmp));
		}| 
	TRUE
		{
			result = new ExpressionResult(true, new TypedValue(Type.BooleanType, (Boolean)true));
		}|
	FALSE
		{
			result = new ExpressionResult(true, new TypedValue(Type.BooleanType, (Boolean)false));
		}|
	result=builtInExpr 
		/*{
			result = new ExpressionResult(false, null);
		}*/|
	/*"NIL" | set |*/
    /*designator*/ procToken=qualident 
    	{
    		v = context.variables.get(context.getVariableId(procToken.getText()));
    	}
    ({v.type.type == BuiltInType.PROCEDURE}?  
    param0Type=actualParameters[procToken] 
    	{
    		context.codeBlock().statements.add(AsmStatement.Call(v.id, procToken));
    		if(!v.type.isInvoke())
    			result = new ExpressionResult(false, new TypedValue(v.type.returnType()));
    		else
    			result = new ExpressionResult(false, new TypedValue(VirtualMachine.invokeReturnType(v, param0Type)));
    	}|
    	{
    		if(v.isConst && v.type.type.canBeConst())
    			result = new ExpressionResult(true, v.value());
    		else
    		{
    			result = new ExpressionResult(false, new TypedValue(v.type));
				context.codeBlock().statements.add(AsmStatement.PushVar(v.id, procToken));
    		}
    	}
    result=arrayIndex[v, result]) | 
    LPAREN result=expression RPAREN | 
    tildeToken:TILDE result=factor
    	{
    		if(result.isConst)
				result.value = Operation.NOT.Perform(result.value);
			else {
				context.codeBlock().statements.add(AsmStatement.UnOp(Operation.NOT, tildeToken));
				result = new ExpressionResult(false, new TypedValue(Operation.NOT.returnType(result.value.type)));
			}
    	};

builtInExpr returns [ExpressionResult result = null] {ExpressionResult arg = null;}:
	oddToken:ODD LPAREN arg=nonConstExpression RPAREN 
		{
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 2l)));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.MOD, oddToken));
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 1l)));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.EQUAL, oddToken));
			result = new ExpressionResult(false, new TypedValue(Operation.EQUAL.returnType(Operation.MOD.returnType(arg.value.type, Type.LongIntType), Type.LongIntType)));
		}| 
	evenToken:EVEN LPAREN arg=nonConstExpression RPAREN 
		{
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 2l)));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.MOD, evenToken));
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.EQUAL, evenToken));
			result = new ExpressionResult(false, new TypedValue(Operation.EQUAL.returnType(Operation.MOD.returnType(arg.value.type, Type.LongIntType), Type.LongIntType)));
		}|
	absToken:ABS LPAREN arg=nonConstExpression RPAREN
		{
			context.codeBlock().statements.add(AsmStatement.Dup(0, absToken));
			context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
			context.codeBlock().statements.add(AsmStatement.BinOp(Operation.GREATER, absToken));
			context.codeBlock().statements.add(AsmStatement.JmpNZ(context.codeBlock().statements.size() + 2, absToken));
			context.codeBlock().statements.add(AsmStatement.UnOp(Operation.MINUS, absToken));
			result = new ExpressionResult(false, new TypedValue(arg.value.type));
		};
		
		
//set  :  LBRACE (element (COMMA element)*)? RBRACE;

//element:  expression (DOTDOT expression)?;

actualParameters[Token procToken] returns [Type param0Type = null]
		{
			Type procVarType = context.variables.get(context.getVariableId(procToken.getText())).type;
		}:
	{procVarType.isRefParam().size() > 0}?
	LPAREN param0Type=actualParamList[procVarType, 0] RPAREN | 
	{procVarType.isRefParam().size() == 0}? (LPAREN RPAREN | );

actualParamList[Type functionType, int pos] returns [Type param0Type = null]:
	param0Type=actualParam[functionType, pos] 
	({functionType.isRefParam().size() > pos + 1}? COMMA actualParamList[functionType, pos + 1] |);
	
actualParam[Type functionType, int pos] returns [Type paramType = null] {ExpressionResult r = null;}:
	{functionType.isRefParam().get(pos)}? paramType=varParam
		{
			if(!functionType.isInvoke() && !paramType.equals(functionType.paramTypes().get(pos)))
				throw new SemanticException("Types of actual and formal var parameters must be identical");
		} |
	{!functionType.isRefParam().get(pos)}? r=nonConstExpression
		{
			if(!functionType.isInvoke() && !r.value.type.convertableTo(functionType.paramTypes().get(pos)))
				throw new SemanticException("Cannot convert \"" + r.value.type + "\" to \"" + functionType.paramTypes().get(pos) + "\"");
			paramType = r.value.type;
		};

varParam returns [Type paramType = null]:
	paramType=lvalue;


















