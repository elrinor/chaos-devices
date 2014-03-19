// $ANTLR : "oberon.g" -> "Oberon2Parser.java"$

package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;
import antlr.TokenStreamRecognitionException;

import antlr.TokenBuffer;
import antlr.TokenStreamException;
import antlr.TokenStreamIOException;
import antlr.ANTLRException;
import antlr.LLkParser;
import antlr.Token;
import antlr.TokenStream;
import antlr.RecognitionException;
import antlr.NoViableAltException;
import antlr.MismatchedTokenException;
import antlr.SemanticException;
import antlr.ParserSharedInputState;
import antlr.collections.impl.BitSet;

public class Oberon2Parser extends antlr.LLkParser       implements Oberon2LexerTokenTypes
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

protected Oberon2Parser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
}

public Oberon2Parser(TokenBuffer tokenBuf) {
  this(tokenBuf,2);
}

protected Oberon2Parser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
}

public Oberon2Parser(TokenStream lexer) {
  this(lexer,2);
}

public Oberon2Parser(ParserSharedInputState state) {
  super(state,2);
  tokenNames = _tokenNames;
}

	public final Token  identdef() throws RecognitionException, TokenStreamException {
		Token token = null;
		
		Token  t = null;
		
		t = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			token = t;
		}
		return token;
	}
	
	public final Token  qualident() throws RecognitionException, TokenStreamException {
		Token token = null;
		
		Token  t1 = null;
		Token  t2 = null;
		boolean isPrefixed = false;
		
		{
		if ((LA(1)==IDENT) && (LA(2)==DOT)) {
			t1 = LT(1);
			match(IDENT);
			match(DOT);
			if ( inputState.guessing==0 ) {
				
							isPrefixed = true;
							token = t1;
						
			}
		}
		else if ((LA(1)==IDENT) && (_tokenSet_0.member(LA(2)))) {
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		}
		t2 = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			
						if(isPrefixed)
							token.setText(token.getText() + "." + t2.getText());
						else
							token = t2;
					
		}
		return token;
	}
	
	public final ArrayList<Token>  identList() throws RecognitionException, TokenStreamException {
		ArrayList<Token> identTokens = new ArrayList<Token>();
		
		Token t;
		
		t=identdef();
		if ( inputState.guessing==0 ) {
			identTokens.add(t);
		}
		{
		_loop3120:
		do {
			if ((LA(1)==COMMA)) {
				match(COMMA);
				t=identdef();
				if ( inputState.guessing==0 ) {
					identTokens.add(t);
				}
			}
			else {
				break _loop3120;
			}
			
		} while (true);
		}
		return identTokens;
	}
	
	public final Type  typeid() throws RecognitionException, TokenStreamException {
		Type type = null;
		
		Token t = null;
		
		t=qualident();
		if ( inputState.guessing==0 ) {
			
						type = context.getType(t.getText());
					
		}
		return type;
	}
	
	public final void module() throws RecognitionException, TokenStreamException {
		
		Token  t1 = null;
		Token  endToken = null;
		Token  t2 = null;
		
					Variable main = null;
				
		
		match(LITERAL_MODULE);
		t1 = LT(1);
		match(IDENT);
		match(SEMICOLON);
		if ( inputState.guessing==0 ) {
			
						main = new Variable(t1.getText(), Type.SimpleProcType, new CodeBlock(), 0, 0, true);
						context.AddVariable(main);
						context.mainId = main.id;
						//context.AddVariable(new Variable(t1, BuiltInType.MODULE, null, true));
					
		}
		{
		switch ( LA(1)) {
		case LITERAL_IMPORT:
		{
			importList();
			break;
		}
		case LITERAL_BEGIN:
		case LITERAL_END:
		case LITERAL_CONST:
		case LITERAL_TYPE:
		case LITERAL_VAR:
		case LITERAL_PROCEDURE:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState.guessing==0 ) {
			
						context.EnterBlock(main);
					
		}
		declarationSequence();
		{
		switch ( LA(1)) {
		case LITERAL_BEGIN:
		{
			match(LITERAL_BEGIN);
			statementSequence();
			break;
		}
		case LITERAL_END:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		endToken = LT(1);
		match(LITERAL_END);
		t2 = LT(1);
		match(IDENT);
		match(DOT);
		if ( inputState.guessing==0 ) {
			
						context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
						context.codeBlock().statements.add(AsmStatement.Exit(endToken));
						if(!t1.getText().equals(t2.getText()))
							throw new SemanticException("Expected \"" + t1.getText() + "\", got \"" + t2.getText() + "\"");
						context.LeaveBlock();
					
		}
	}
	
	public final void importList() throws RecognitionException, TokenStreamException {
		
		
		match(LITERAL_IMPORT);
		obimport();
		{
		_loop3127:
		do {
			if ((LA(1)==COMMA)) {
				match(COMMA);
				obimport();
			}
			else {
				break _loop3127;
			}
			
		} while (true);
		}
		match(SEMICOLON);
	}
	
	public final void declarationSequence() throws RecognitionException, TokenStreamException {
		
		
		{
		_loop3139:
		do {
			switch ( LA(1)) {
			case LITERAL_CONST:
			{
				match(LITERAL_CONST);
				{
				_loop3133:
				do {
					if ((LA(1)==IDENT)) {
						constantDeclaration();
						match(SEMICOLON);
					}
					else {
						break _loop3133;
					}
					
				} while (true);
				}
				break;
			}
			case LITERAL_TYPE:
			{
				match(LITERAL_TYPE);
				{
				_loop3135:
				do {
					if ((LA(1)==IDENT)) {
						typeDeclaration();
						match(SEMICOLON);
					}
					else {
						break _loop3135;
					}
					
				} while (true);
				}
				break;
			}
			case LITERAL_VAR:
			{
				match(LITERAL_VAR);
				{
				_loop3137:
				do {
					if ((LA(1)==IDENT)) {
						variableDeclaration();
						match(SEMICOLON);
					}
					else {
						break _loop3137;
					}
					
				} while (true);
				}
				break;
			}
			case LITERAL_PROCEDURE:
			{
				{
				if ((LA(1)==LITERAL_PROCEDURE) && (LA(2)==IDENT)) {
					procedureDeclaration();
					match(SEMICOLON);
				}
				else if ((LA(1)==LITERAL_PROCEDURE) && (LA(2)==REF)) {
					forwardDeclaration();
					match(SEMICOLON);
				}
				else {
					throw new NoViableAltException(LT(1), getFilename());
				}
				
				}
				break;
			}
			default:
			{
				break _loop3139;
			}
			}
		} while (true);
		}
	}
	
	public final void statementSequence() throws RecognitionException, TokenStreamException {
		
		
		statement();
		{
		_loop3167:
		do {
			if ((LA(1)==SEMICOLON)) {
				match(SEMICOLON);
				statement();
			}
			else {
				break _loop3167;
			}
			
		} while (true);
		}
	}
	
	public final void obimport() throws RecognitionException, TokenStreamException {
		
		Token  t1 = null;
		Token  t2 = null;
		boolean importedAs = false;
		
		t1 = LT(1);
		match(IDENT);
		{
		switch ( LA(1)) {
		case ASSIGN:
		{
			match(ASSIGN);
			t2 = LT(1);
			match(IDENT);
			if ( inputState.guessing==0 ) {
				
							importedAs = true;
							context.ImportModule(t2, t1);
						
			}
			break;
		}
		case COMMA:
		case SEMICOLON:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState.guessing==0 ) {
			
						if(!importedAs)
							context.ImportModule(t1);
					
		}
	}
	
	public final void constantDeclaration() throws RecognitionException, TokenStreamException {
		
		TypedValue value = null; Token t = null;
		
		t=identdef();
		match(EQUAL);
		value=constExpression();
		if ( inputState.guessing==0 ) {
			
						context.AddVariable(new Variable(t, value, true));
					
		}
	}
	
	public final void typeDeclaration() throws RecognitionException, TokenStreamException {
		
		Token nameToken = null; Type type = null;
		
		nameToken=identdef();
		match(EQUAL);
		type=typedef();
		if ( inputState.guessing==0 ) {
			
						type.name = nameToken.getText();
						context.AddType(type);
					
		}
	}
	
	public final void variableDeclaration() throws RecognitionException, TokenStreamException {
		
		ArrayList<Token> varTokens = null; Type type = null;
		
		varTokens=identList();
		match(COLON);
		type=typedef();
		if ( inputState.guessing==0 ) {
			
						for(int i = 0; i < varTokens.size(); i++)
							context.AddVariable(new Variable(varTokens.get(i), type, null, false));
					
		}
	}
	
	public final void procedureDeclaration() throws RecognitionException, TokenStreamException {
		
		Token  endToken = null;
		Token  t = null;
		
					Token nameToken = null; 
					Pair<Type, ArrayList<Token>> procType = new Pair<Type, ArrayList<Token>>(Type.SimpleProcType, null); 
					CodeBlock block = null;
					Variable procVar = null;
				
		
		match(LITERAL_PROCEDURE);
		nameToken=identdef();
		procType=formalParameters();
		match(SEMICOLON);
		if ( inputState.guessing==0 ) {
			
						procVar = new Variable(nameToken, procType.first, new CodeBlock(), true);
						context.AddVariable(procVar);
						context.EnterBlock(procVar);
						if(procType.second != null)
							for(int i = 0; i < procType.second.size(); i++)
								context.AddVariable(new Variable(procType.second.get(i), procType.first.paramTypes().get(i), null, false));
					
		}
		declarationSequence();
		{
		switch ( LA(1)) {
		case LITERAL_BEGIN:
		{
			match(LITERAL_BEGIN);
			statementSequence();
			break;
		}
		case LITERAL_END:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		endToken = LT(1);
		match(LITERAL_END);
		t = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			
						if(!nameToken.getText().equals(t.getText()))
							throw new SemanticException("Expected \"" + nameToken.getText() + "\", got \"" + t.getText() + "\"");
						context.codeBlock().statements.add(AsmStatement.Push(TypedValue.VoidTypedValue));
						context.codeBlock().statements.add(AsmStatement.Ret(endToken));
						context.LeaveBlock();
					
		}
	}
	
	public final void forwardDeclaration() throws RecognitionException, TokenStreamException {
		
		Token  t = null;
		
					Pair<Type, ArrayList<Token>> procType = new Pair<Type, ArrayList<Token>>(Type.SimpleProcType, null);
				
		
		match(LITERAL_PROCEDURE);
		match(REF);
		t = LT(1);
		match(IDENT);
		procType=formalParameters();
		if ( inputState.guessing==0 ) {
			
						context.AddVariable(new Variable(t, procType.first, null, true));
					
		}
	}
	
	public final Pair<Type, ArrayList<Token>>  formalParameters() throws RecognitionException, TokenStreamException {
		Pair<Type, ArrayList<Token>> result = null;
		
		
					Triad<ArrayList<Type>, ArrayList<Boolean>, ArrayList<Token>> tmpParams = null;
					ArrayList<Type> paramTypes = new ArrayList<Type>();
					ArrayList<Boolean> isRefParam = new ArrayList<Boolean>();
					ArrayList<Token> tokenList = new ArrayList<Token>();
					Type returnType = Type.VoidType;
				
		
		{
		switch ( LA(1)) {
		case LPAREN:
		{
			match(LPAREN);
			{
			switch ( LA(1)) {
			case IDENT:
			case LITERAL_VAR:
			{
				tmpParams=fPSection();
				if ( inputState.guessing==0 ) {
					
								paramTypes.addAll(tmpParams.first);
								isRefParam.addAll(tmpParams.second);
								tokenList.addAll(tmpParams.third);
							
				}
				{
				_loop3147:
				do {
					if ((LA(1)==SEMICOLON)) {
						match(SEMICOLON);
						tmpParams=fPSection();
						if ( inputState.guessing==0 ) {
							
										paramTypes.addAll(tmpParams.first);
										isRefParam.addAll(tmpParams.second);
										tokenList.addAll(tmpParams.third);
									
						}
					}
					else {
						break _loop3147;
					}
					
				} while (true);
				}
				break;
			}
			case RPAREN:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			match(RPAREN);
			break;
		}
		case SEMICOLON:
		case COLON:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{
		switch ( LA(1)) {
		case COLON:
		{
			match(COLON);
			returnType=typeid();
			break;
		}
		case SEMICOLON:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState.guessing==0 ) {
			
					if(returnType.type == BuiltInType.ARRAY)
						throw new SemanticException("Cannot return array from a function");
					result = new Pair(new Type(BuiltInType.PROCEDURE, paramTypes, isRefParam, returnType), tokenList);
				
		}
		return result;
	}
	
	public final Triad<ArrayList<Type>, ArrayList<Boolean>, ArrayList<Token>>  fPSection() throws RecognitionException, TokenStreamException {
		Triad<ArrayList<Type>, ArrayList<Boolean>, ArrayList<Token>> result = null;
		
		Token  t1 = null;
		Token  t2 = null;
		
					ArrayList<Token> paramTokens = new ArrayList<Token>();
					boolean isRefParam = false;
					Type type = null;
				
		
		{
		switch ( LA(1)) {
		case LITERAL_VAR:
		{
			match(LITERAL_VAR);
			if ( inputState.guessing==0 ) {
				isRefParam = true;
			}
			break;
		}
		case IDENT:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		t1 = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			paramTokens.add(t1);
		}
		{
		_loop3152:
		do {
			if ((LA(1)==COMMA)) {
				match(COMMA);
				t2 = LT(1);
				match(IDENT);
				if ( inputState.guessing==0 ) {
					paramTokens.add(t2);
				}
			}
			else {
				break _loop3152;
			}
			
		} while (true);
		}
		match(COLON);
		type=formalType();
		if ( inputState.guessing==0 ) {
			
						ArrayList<Type> typeList = new ArrayList<Type>();
						ArrayList<Boolean> boolList = new ArrayList<Boolean>();
						for(int i = 0; i < paramTokens.size(); i++)
						{
							boolList.add(isRefParam);
							typeList.add(type);
						}
						result = new Triad<ArrayList<Type>, ArrayList<Boolean>, ArrayList<Token>>(typeList, boolList, paramTokens);
					
		}
		return result;
	}
	
	public final Type  formalType() throws RecognitionException, TokenStreamException {
		Type type = null;
		
		int arrayCount = 0;
		
		{
		_loop3155:
		do {
			if ((LA(1)==LITERAL_ARRAY)) {
				match(LITERAL_ARRAY);
				match(LITERAL_OF);
				if ( inputState.guessing==0 ) {
					arrayCount++;
				}
			}
			else {
				break _loop3155;
			}
			
		} while (true);
		}
		{
		type=typeid();
		}
		if ( inputState.guessing==0 ) {
			
						while(arrayCount-- > 0)
							type = new Type(BuiltInType.ARRAY, type, 0);
					
		}
		return type;
	}
	
	public final TypedValue  constExpression() throws RecognitionException, TokenStreamException {
		TypedValue result = null;
		
		
					ExpressionResult expr = null;
				
		
		expr=expression();
		if ( inputState.guessing==0 ) {
			
						if(!expr.isConst)
							throw new SemanticException("Constant expected");
						result = expr.value;
					
		}
		return result;
	}
	
	public final Type  typedef() throws RecognitionException, TokenStreamException {
		Type type = null;
		
		Token nameToken = null;
		
		switch ( LA(1)) {
		case IDENT:
		{
			nameToken=qualident();
			if ( inputState.guessing==0 ) {
				type = context.getType(nameToken.getText());
			}
			break;
		}
		case LITERAL_ARRAY:
		{
			type=arrayType();
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return type;
	}
	
	public final Type  arrayType() throws RecognitionException, TokenStreamException {
		Type type = null;
		
		
					ArrayList<Integer> lengths = new ArrayList<Integer>(); 
					int currentLenght = 0;
				
		
		match(LITERAL_ARRAY);
		currentLenght=length();
		if ( inputState.guessing==0 ) {
			lengths.add(currentLenght);
		}
		{
		_loop3162:
		do {
			if ((LA(1)==COMMA)) {
				match(COMMA);
				currentLenght=length();
				if ( inputState.guessing==0 ) {
					lengths.add(currentLenght);
				}
			}
			else {
				break _loop3162;
			}
			
		} while (true);
		}
		match(LITERAL_OF);
		type=typedef();
		if ( inputState.guessing==0 ) {
			
						for(int i = lengths.size() - 1; i >= 0; i--)
							type = new Type(BuiltInType.ARRAY, type, lengths.get(i));
					
		}
		return type;
	}
	
	public final int  length() throws RecognitionException, TokenStreamException {
		int result = 0;
		
		TypedValue value = null;
		
		value=constExpression();
		if ( inputState.guessing==0 ) {
			
						result = (Integer)value.ConvertTo(Type.IntegerType).value;
						if(result <= 0)
							throw new SemanticException("Cannot define array of non-positive length");
					
		}
		return result;
	}
	
	public final void statement() throws RecognitionException, TokenStreamException {
		
		
		{
		switch ( LA(1)) {
		case IDENT:
		{
			simpleStatement();
			break;
		}
		case LITERAL_FOR:
		{
			forStatement();
			break;
		}
		case LITERAL_IF:
		{
			ifStatement();
			break;
		}
		case LITERAL_CASE:
		{
			caseStatement();
			break;
		}
		case LITERAL_WHILE:
		{
			whileStatement();
			break;
		}
		case LITERAL_REPEAT:
		{
			repeatStatement();
			break;
		}
		case LITERAL_LOOP:
		{
			loopStatement();
			break;
		}
		case LITERAL_RETURN:
		{
			returnStatement();
			break;
		}
		case BREAK:
		case LITERAL_EXIT:
		{
			breakStatement();
			break;
		}
		case CONTINUE:
		{
			continueStatement();
			break;
		}
		case INC:
		case DEC:
		case HALT:
		case ODD:
		case EVEN:
		case ABS:
		{
			builtInProc();
			break;
		}
		case SEMICOLON:
		case LITERAL_END:
		case LITERAL_ELSIF:
		case LITERAL_ELSE:
		case LITERAL_UNTIL:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
	}
	
	public final void simpleStatement() throws RecognitionException, TokenStreamException {
		
		
		boolean synPredMatched3180 = false;
		if (((LA(1)==IDENT) && (LA(2)==DOT||LA(2)==ASSIGN||LA(2)==LBR))) {
			int _m3180 = mark();
			synPredMatched3180 = true;
			inputState.guessing++;
			try {
				{
				qualident();
				{
				switch ( LA(1)) {
				case ASSIGN:
				{
					match(ASSIGN);
					break;
				}
				case LBR:
				{
					match(LBR);
					break;
				}
				default:
				{
					throw new NoViableAltException(LT(1), getFilename());
				}
				}
				}
				}
			}
			catch (RecognitionException pe) {
				synPredMatched3180 = false;
			}
			rewind(_m3180);
inputState.guessing--;
		}
		if ( synPredMatched3180 ) {
			assignment();
		}
		else if ((LA(1)==IDENT) && (_tokenSet_1.member(LA(2)))) {
			procedureCall();
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
	}
	
	public final void forStatement() throws RecognitionException, TokenStreamException {
		
		Token  forToken = null;
		
					int jmpToEndPosition = 0;
					int jmpNZToIncPosition = 0;
				
		
		forToken = LT(1);
		match(LITERAL_FOR);
		if ( inputState.guessing==0 ) {
			
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
		lvalue();
		if ( inputState.guessing==0 ) {
			
						context.codeBlock().statements.add(AsmStatement.Dup(1, forToken));
						jmpNZToIncPosition = context.codeBlock().statements.size();
						context.codeBlock().statements.add(AsmStatement.JmpNZ(0, forToken)); // To INC -- fixed later
						// Init
						context.codeBlock().statements.add(AsmStatement.Dup(0, forToken));
					
		}
		match(ASSIGN);
		nonConstExpression();
		match(LITERAL_TO);
		if ( inputState.guessing==0 ) {
			
						context.codeBlock().statements.add(AsmStatement.BinOp(Operation.ASSIGN, forToken));
						context.codeBlock().statements.add(AsmStatement.Pop(forToken));
					
		}
		nonConstExpression();
		{
		switch ( LA(1)) {
		case LITERAL_BY:
		{
			match(LITERAL_BY);
			nonConstExpression();
			break;
		}
		case LITERAL_DO:
		{
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.ShortIntType, (Short)(short)1)));
						
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState.guessing==0 ) {
			
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
		match(LITERAL_DO);
		statementSequence();
		match(LITERAL_END);
		if ( inputState.guessing==0 ) {
			
						context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition + 1));
						context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
						context.LeaveBeginEndBlock();
					
		}
	}
	
	public final void ifStatement() throws RecognitionException, TokenStreamException {
		
		
		match(LITERAL_IF);
		ifFromExpr();
		match(LITERAL_END);
	}
	
	public final void caseStatement() throws RecognitionException, TokenStreamException {
		
		
		match(LITERAL_CASE);
	}
	
	public final void whileStatement() throws RecognitionException, TokenStreamException {
		
		Token  doToken = null;
		
					int exprPosition = context.codeBlock().statements.size();
					int jmpToEndPosition = 0;
					Token firstToken = null;
				
		
		match(LITERAL_WHILE);
		if ( inputState.guessing==0 ) {
			
						firstToken = LT(1);
					
		}
		boolExpression();
		doToken = LT(1);
		match(LITERAL_DO);
		if ( inputState.guessing==0 ) {
			
						jmpToEndPosition = context.codeBlock().statements.size() + 3;
						context.codeBlock().statements.add(AsmStatement.JmpNZ(jmpToEndPosition + 1, firstToken));
						context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition));
						context.codeBlock().statements.add(AsmStatement.Jmp(exprPosition));
						context.codeBlock().statements.add(AsmStatement.Jmp(0));
						context.EnterBeginEndBlock(jmpToEndPosition);
					
		}
		statementSequence();
		match(LITERAL_END);
		if ( inputState.guessing==0 ) {
			
						context.codeBlock().statements.add(AsmStatement.Jmp(exprPosition));
						context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
						context.LeaveBeginEndBlock();
					
		}
	}
	
	public final void repeatStatement() throws RecognitionException, TokenStreamException {
		
		
					int jmpToEndPosition = 0;
					Token firstToken = null;
				
		
		match(LITERAL_REPEAT);
		if ( inputState.guessing==0 ) {
			
						jmpToEndPosition = context.codeBlock().statements.size() + 1;
						context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition + 1));
						context.codeBlock().statements.add(AsmStatement.Jmp(0));
						context.EnterBeginEndBlock(jmpToEndPosition);
					
		}
		statementSequence();
		match(LITERAL_UNTIL);
		if ( inputState.guessing==0 ) {
			
						firstToken = LT(1);
					
		}
		boolExpression();
		if ( inputState.guessing==0 ) {
			
						context.codeBlock().statements.add(AsmStatement.JmpNZ(jmpToEndPosition, firstToken));
						context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition - 1));
						context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
						context.LeaveBeginEndBlock();
					
		}
	}
	
	public final void loopStatement() throws RecognitionException, TokenStreamException {
		
		
					int jmpToEndPosition = 0;
				
		
		match(LITERAL_LOOP);
		if ( inputState.guessing==0 ) {
			
						jmpToEndPosition = context.codeBlock().statements.size() + 1;
						context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition + 1));
						context.codeBlock().statements.add(AsmStatement.Jmp(0));
						context.EnterBeginEndBlock(jmpToEndPosition);
					
		}
		statementSequence();
		match(LITERAL_END);
		if ( inputState.guessing==0 ) {
			
						context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition - 1));
						context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
						context.LeaveBeginEndBlock();
					
		}
	}
	
	public final void returnStatement() throws RecognitionException, TokenStreamException {
		
		Token  retToken = null;
		
					boolean returned = false;
					ExpressionResult r = new ExpressionResult(false, new TypedValue(Type.VoidType));
				
		
		retToken = LT(1);
		match(LITERAL_RETURN);
		{
		switch ( LA(1)) {
		case ODD:
		case EVEN:
		case ABS:
		case TRUE:
		case FALSE:
		case LPAREN:
		case PLUS:
		case MINUS:
		case TILDE:
		case INT:
		case FLOAT:
		case CHAR:
		case STRING:
		case IDENT:
		{
			r=nonConstExpression();
			if ( inputState.guessing==0 ) {
				returned = true;
			}
			break;
		}
		case SEMICOLON:
		case LITERAL_END:
		case LITERAL_ELSIF:
		case LITERAL_ELSE:
		case LITERAL_UNTIL:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState.guessing==0 ) {
			
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
					
		}
	}
	
	public final void breakStatement() throws RecognitionException, TokenStreamException {
		
		Token  breakToken = null;
		Token  exitToken = null;
		
		{
		switch ( LA(1)) {
		case BREAK:
		{
			breakToken = LT(1);
			match(BREAK);
			break;
		}
		case LITERAL_EXIT:
		{
			exitToken = LT(1);
			match(LITERAL_EXIT);
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState.guessing==0 ) {
			
						if(breakToken == null)
							breakToken = exitToken;
						if(context.CurrentBlockJmpToEndPosition() != null)
							context.codeBlock().statements.add(AsmStatement.Jmp(context.CurrentBlockJmpToEndPosition()));
						else
							throw new SemanticException("No cycle to leave");
					
		}
	}
	
	public final void continueStatement() throws RecognitionException, TokenStreamException {
		
		Token  contToken = null;
		
		contToken = LT(1);
		match(CONTINUE);
		if ( inputState.guessing==0 ) {
			
						if(context.CurrentBlockJmpToEndPosition() != null)
							context.codeBlock().statements.add(AsmStatement.Jmp(context.CurrentBlockJmpToEndPosition() - 1));
						else
							throw new SemanticException("No cycle to continue");
					
		}
	}
	
	public final void builtInProc() throws RecognitionException, TokenStreamException {
		
		Token  incToken = null;
		Token  decToken = null;
		Token  haltToken = null;
		
					Token beginToken = null;
					Type lValueType = null;
					ExpressionResult r = new ExpressionResult(false, new TypedValue(Type.ShortIntType));
				
		
		switch ( LA(1)) {
		case INC:
		{
			incToken = LT(1);
			match(INC);
			match(LPAREN);
			lValueType=varParam();
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.Dup(0, incToken));
						
			}
			{
			switch ( LA(1)) {
			case COMMA:
			{
				match(COMMA);
				r=nonConstExpression();
				break;
			}
			case RPAREN:
			{
				if ( inputState.guessing==0 ) {
					
								context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.ShortIntType, (Short)(short)1)));
							
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.BinOp(Operation.PLUS, incToken));
							context.codeBlock().statements.add(AsmStatement.BinOp(Operation.ASSIGN, incToken));
							context.codeBlock().statements.add(AsmStatement.Pop(incToken));
							Operation.ASSIGN.returnType(lValueType, Operation.PLUS.returnType(lValueType, r.value.type));
						
			}
			break;
		}
		case DEC:
		{
			decToken = LT(1);
			match(DEC);
			match(LPAREN);
			lValueType=varParam();
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.Dup(0, decToken));
						
			}
			{
			switch ( LA(1)) {
			case COMMA:
			{
				match(COMMA);
				r=nonConstExpression();
				break;
			}
			case RPAREN:
			{
				if ( inputState.guessing==0 ) {
					
								context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.ShortIntType, (Short)(short)1)));
							
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.BinOp(Operation.MINUS, decToken));
							context.codeBlock().statements.add(AsmStatement.BinOp(Operation.ASSIGN, decToken));
							context.codeBlock().statements.add(AsmStatement.Pop(decToken));
							Operation.ASSIGN.returnType(lValueType, Operation.PLUS.returnType(lValueType, r.value.type));
						
			}
			break;
		}
		case HALT:
		{
			haltToken = LT(1);
			match(HALT);
			{
			switch ( LA(1)) {
			case LPAREN:
			{
				match(LPAREN);
				r=nonConstExpression();
				match(RPAREN);
				if ( inputState.guessing==0 ) {
					
								if(!r.value.type.convertableTo(Type.LongIntType))
									throw new SemanticException("Cannot convert \"" + r.value.type + "\" to \"" + Type.LongIntType + "\"");
							
				}
				break;
			}
			case SEMICOLON:
			case LITERAL_END:
			case LITERAL_ELSIF:
			case LITERAL_ELSE:
			case LITERAL_UNTIL:
			{
				if ( inputState.guessing==0 ) {
					
								context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
							
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.Exit(haltToken));
						
			}
			break;
		}
		case ODD:
		case EVEN:
		case ABS:
		{
			if ( inputState.guessing==0 ) {
				
							beginToken = LT(0);
						
			}
			r=builtInExpr();
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.Pop(beginToken));
						
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
	}
	
	public final Type  varParam() throws RecognitionException, TokenStreamException {
		Type paramType = null;
		
		
		paramType=lvalue();
		return paramType;
	}
	
	public final ExpressionResult  nonConstExpression() throws RecognitionException, TokenStreamException {
		ExpressionResult result = null;
		
		
		result=expression();
		if ( inputState.guessing==0 ) {
			
						if(result.isConst) {
							context.codeBlock().statements.add(AsmStatement.Push(result.value));
							result.isConst = false;
						}
					
		}
		return result;
	}
	
	public final ExpressionResult  builtInExpr() throws RecognitionException, TokenStreamException {
		ExpressionResult result = null;
		
		Token  oddToken = null;
		Token  evenToken = null;
		Token  absToken = null;
		ExpressionResult arg = null;
		
		switch ( LA(1)) {
		case ODD:
		{
			oddToken = LT(1);
			match(ODD);
			match(LPAREN);
			arg=nonConstExpression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 2l)));
							context.codeBlock().statements.add(AsmStatement.BinOp(Operation.MOD, oddToken));
							context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 1l)));
							context.codeBlock().statements.add(AsmStatement.BinOp(Operation.EQUAL, oddToken));
							result = new ExpressionResult(false, new TypedValue(Operation.EQUAL.returnType(Operation.MOD.returnType(arg.value.type, Type.LongIntType), Type.LongIntType)));
						
			}
			break;
		}
		case EVEN:
		{
			evenToken = LT(1);
			match(EVEN);
			match(LPAREN);
			arg=nonConstExpression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 2l)));
							context.codeBlock().statements.add(AsmStatement.BinOp(Operation.MOD, evenToken));
							context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
							context.codeBlock().statements.add(AsmStatement.BinOp(Operation.EQUAL, evenToken));
							result = new ExpressionResult(false, new TypedValue(Operation.EQUAL.returnType(Operation.MOD.returnType(arg.value.type, Type.LongIntType), Type.LongIntType)));
						
			}
			break;
		}
		case ABS:
		{
			absToken = LT(1);
			match(ABS);
			match(LPAREN);
			arg=nonConstExpression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.add(AsmStatement.Dup(0, absToken));
							context.codeBlock().statements.add(AsmStatement.Push(new TypedValue(Type.LongIntType, 0l)));
							context.codeBlock().statements.add(AsmStatement.BinOp(Operation.GREATER, absToken));
							context.codeBlock().statements.add(AsmStatement.JmpNZ(context.codeBlock().statements.size() + 2, absToken));
							context.codeBlock().statements.add(AsmStatement.UnOp(Operation.MINUS, absToken));
							result = new ExpressionResult(false, new TypedValue(arg.value.type));
						
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return result;
	}
	
	public final void assignment() throws RecognitionException, TokenStreamException {
		
		Token  asgnToken = null;
		Type lValueType = null; ExpressionResult r = null;
		
		lValueType=lvalue();
		asgnToken = LT(1);
		match(ASSIGN);
		r=nonConstExpression();
		if ( inputState.guessing==0 ) {
			
					context.codeBlock().statements.add(AsmStatement.BinOp(Operation.ASSIGN, asgnToken));
					context.codeBlock().statements.add(AsmStatement.Pop(asgnToken)); // Pop Void from Assign
					errToken = asgnToken;
					Operation.ASSIGN.returnType(lValueType, r.value.type);
					errToken = null;
					
		}
	}
	
	public final void procedureCall() throws RecognitionException, TokenStreamException {
		
		
					Token procToken = null;
					Type param0Type = null;
				
		
		procToken=qualident();
		param0Type=actualParameters(procToken);
		if ( inputState.guessing==0 ) {
			
						Variable proc = context.variables.get(context.getVariableId(procToken.getText()));
					if(proc.type.type != BuiltInType.PROCEDURE)
						throw new SemanticException("\"" + procToken.getText() + "\" is not a procedure");
					context.codeBlock().statements.add(AsmStatement.Call(proc.id, procToken));
					context.codeBlock().statements.add(AsmStatement.Pop(procToken)); // pop the result we don't need
					
		}
	}
	
	public final Type  actualParameters(
		Token procToken
	) throws RecognitionException, TokenStreamException {
		Type param0Type = null;
		
		
					Type procVarType = context.variables.get(context.getVariableId(procToken.getText())).type;
				
		
		if (((LA(1)==LPAREN) && (_tokenSet_2.member(LA(2))))&&(procVarType.isRefParam().size() > 0)) {
			match(LPAREN);
			param0Type=actualParamList(procVarType, 0);
			match(RPAREN);
		}
		else if (((_tokenSet_3.member(LA(1))) && (_tokenSet_4.member(LA(2))))&&(procVarType.isRefParam().size() == 0)) {
			{
			switch ( LA(1)) {
			case LPAREN:
			{
				match(LPAREN);
				match(RPAREN);
				break;
			}
			case STAR:
			case EQUAL:
			case COMMA:
			case SEMICOLON:
			case RPAREN:
			case RBR:
			case NEQUAL:
			case LEQ:
			case LESS:
			case GEQ:
			case GREATER:
			case PLUS:
			case MINUS:
			case SLASH:
			case AMP:
			case LITERAL_END:
			case LITERAL_OF:
			case LITERAL_THEN:
			case LITERAL_ELSIF:
			case LITERAL_ELSE:
			case LITERAL_DO:
			case LITERAL_UNTIL:
			case LITERAL_TO:
			case LITERAL_BY:
			case LITERAL_OR:
			case LITERAL_DIV:
			case LITERAL_MOD:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		return param0Type;
	}
	
	public final Type  lvalue() throws RecognitionException, TokenStreamException {
		Type type = null;
		
		
					Token t = null;
					ExpressionResult r = null;
					Variable var = null;
				
		
		t=qualident();
		if ( inputState.guessing==0 ) {
			
						var = context.variables.get(context.getVariableId(t.getText()));
						if(var.isConst)
							throw new SemanticException("Can't assign to a constant");
						context.codeBlock().statements.add(AsmStatement.PushVar(var.id, t));
						r = new ExpressionResult(false, new TypedValue(var.type));
					
		}
		r=arrayIndex(var, r);
		if ( inputState.guessing==0 ) {
			
						type = r.value.type;
					
		}
		return type;
	}
	
	public final ExpressionResult  arrayIndex(
		Variable v, ExpressionResult defResult
	) throws RecognitionException, TokenStreamException {
		ExpressionResult result = null;
		
		Token  lbrToken = null;
		
				ExpressionResult r = null;
			
		
		if ( inputState.guessing==0 ) {
			
					result = defResult;
				
		}
		{
		_loop3190:
		do {
			if ((LA(1)==LBR)) {
				lbrToken = LT(1);
				match(LBR);
				r=nonConstExpression();
				if ( inputState.guessing==0 ) {
					
								context.codeBlock().statements.add(AsmStatement.BinOp(Operation.INDEX, lbrToken));
								result.value.type = Operation.INDEX.returnType(result.value.type, r.value.type);
							
				}
				{
				_loop3189:
				do {
					if ((LA(1)==COMMA)) {
						match(COMMA);
						r=nonConstExpression();
						if ( inputState.guessing==0 ) {
							
										context.codeBlock().statements.add(AsmStatement.BinOp(Operation.INDEX, lbrToken));
										result.value.type = Operation.INDEX.returnType(result.value.type, r.value.type);
									
						}
					}
					else {
						break _loop3189;
					}
					
				} while (true);
				}
				match(RBR);
			}
			else {
				break _loop3190;
			}
			
		} while (true);
		}
		return result;
	}
	
	public final void ifFromExpr() throws RecognitionException, TokenStreamException {
		
		
					int jmpToEndPosition = 0;
					Token firstToken = LT(1);
				
		
		boolExpression();
		match(LITERAL_THEN);
		if ( inputState.guessing==0 ) {
			
						jmpToEndPosition = context.codeBlock().statements.size() + 2;
						context.codeBlock().statements.add(AsmStatement.JmpNZ(jmpToEndPosition + 1, firstToken));
						context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition)); // Skip else for now
						context.codeBlock().statements.add(AsmStatement.Jmp(0));
					
		}
		statementSequence();
		if ( inputState.guessing==0 ) {
			
						context.codeBlock().statements.add(AsmStatement.Jmp(jmpToEndPosition));
					
		}
		{
		switch ( LA(1)) {
		case LITERAL_ELSIF:
		case LITERAL_ELSE:
		{
			if ( inputState.guessing==0 ) {
				
							context.codeBlock().statements.set(jmpToEndPosition - 1, AsmStatement.Jmp(context.codeBlock().statements.size()));
						
			}
			{
			switch ( LA(1)) {
			case LITERAL_ELSIF:
			{
				match(LITERAL_ELSIF);
				ifFromExpr();
				break;
			}
			case LITERAL_ELSE:
			{
				match(LITERAL_ELSE);
				statementSequence();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			break;
		}
		case LITERAL_END:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState.guessing==0 ) {
			
						context.codeBlock().statements.set(jmpToEndPosition, AsmStatement.Jmp(context.codeBlock().statements.size()));
					
		}
	}
	
	public final void boolExpression() throws RecognitionException, TokenStreamException {
		
		ExpressionResult r = null;
		
		r=nonConstExpression();
		if ( inputState.guessing==0 ) {
			
						if(r.value.type.type != BuiltInType.BOOLEAN)
							throw new SemanticException("Boolean expression expected");
					
		}
	}
	
	public final ExpressionResult  expression() throws RecognitionException, TokenStreamException {
		ExpressionResult result = null;
		
		
					Operation op = null;
					ExpressionResult result2 = null;
					Token opToken = null;
				
		
		result=simpleExpression();
		if ( inputState.guessing==0 ) {
			
						if(result.isConst)
							context.codeBlock().statements.add(AsmStatement.Push(result.value));
					
		}
		{
		switch ( LA(1)) {
		case EQUAL:
		case NEQUAL:
		case LEQ:
		case LESS:
		case GEQ:
		case GREATER:
		{
			if ( inputState.guessing==0 ) {
				
							opToken = LT(1);
						
			}
			op=relation();
			result2=simpleExpression();
			if ( inputState.guessing==0 ) {
				
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
			break;
		}
		case COMMA:
		case SEMICOLON:
		case RPAREN:
		case RBR:
		case LITERAL_END:
		case LITERAL_OF:
		case LITERAL_THEN:
		case LITERAL_ELSIF:
		case LITERAL_ELSE:
		case LITERAL_DO:
		case LITERAL_UNTIL:
		case LITERAL_TO:
		case LITERAL_BY:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		if ( inputState.guessing==0 ) {
			
						if(result.isConst)
							context.codeBlock().removeLastStatement();
					
		}
		return result;
	}
	
	public final ExpressionResult  simpleExpression() throws RecognitionException, TokenStreamException {
		ExpressionResult result = null;
		
		
					Operation op = null;
					Operation unop = null;
					ExpressionResult result2 = null;
					Token opToken = LT(1);
				
		
		{
		switch ( LA(1)) {
		case PLUS:
		case MINUS:
		{
			unop=plusMinus();
			break;
		}
		case ODD:
		case EVEN:
		case ABS:
		case TRUE:
		case FALSE:
		case LPAREN:
		case TILDE:
		case INT:
		case FLOAT:
		case CHAR:
		case STRING:
		case IDENT:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		result=term();
		if ( inputState.guessing==0 ) {
			
						if(result.isConst)
						{
							if(unop != null && unop != Operation.PLUS)
								result.value = unop.Perform(result.value);
							context.codeBlock().statements.add(AsmStatement.Push(result.value));
						}
						else if(unop != null && unop != Operation.PLUS)
							context.codeBlock().statements.add(AsmStatement.UnOp(unop, opToken));
					
		}
		{
		_loop3210:
		do {
			if ((LA(1)==PLUS||LA(1)==MINUS||LA(1)==LITERAL_OR)) {
				if ( inputState.guessing==0 ) {
					
								opToken = LT(1);
							
				}
				op=addOperator();
				result2=term();
				if ( inputState.guessing==0 ) {
					
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
			}
			else {
				break _loop3210;
			}
			
		} while (true);
		}
		if ( inputState.guessing==0 ) {
			
						if(result.isConst)
							context.codeBlock().removeLastStatement();
					
		}
		return result;
	}
	
	public final Operation  relation() throws RecognitionException, TokenStreamException {
		Operation op = null;
		
		
		switch ( LA(1)) {
		case EQUAL:
		{
			match(EQUAL);
			if ( inputState.guessing==0 ) {
				op = Operation.EQUAL;
			}
			break;
		}
		case NEQUAL:
		{
			match(NEQUAL);
			if ( inputState.guessing==0 ) {
				op = Operation.NOTEQUAL;
			}
			break;
		}
		case LESS:
		{
			match(LESS);
			if ( inputState.guessing==0 ) {
				op = Operation.LESS;
			}
			break;
		}
		case LEQ:
		{
			match(LEQ);
			if ( inputState.guessing==0 ) {
				op = Operation.LESSOREQUAL;
			}
			break;
		}
		case GREATER:
		{
			match(GREATER);
			if ( inputState.guessing==0 ) {
				op = Operation.GREATER;
			}
			break;
		}
		case GEQ:
		{
			match(GEQ);
			if ( inputState.guessing==0 ) {
				op = Operation.GREATEROREQUAL;
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return op;
	}
	
	public final Operation  plusMinus() throws RecognitionException, TokenStreamException {
		Operation op = null;
		
		
		switch ( LA(1)) {
		case PLUS:
		{
			match(PLUS);
			if ( inputState.guessing==0 ) {
				op = Operation.PLUS;
			}
			break;
		}
		case MINUS:
		{
			match(MINUS);
			if ( inputState.guessing==0 ) {
				op = Operation.MINUS;
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return op;
	}
	
	public final ExpressionResult  term() throws RecognitionException, TokenStreamException {
		ExpressionResult result = null;
		
		
					Operation op = null;
					ExpressionResult result2 = null;
					Token opToken = null;
				
		
		result=factor();
		if ( inputState.guessing==0 ) {
			
						if(result.isConst)
							context.codeBlock().statements.add(AsmStatement.Push(result.value));
					
		}
		{
		_loop3215:
		do {
			if ((_tokenSet_5.member(LA(1)))) {
				if ( inputState.guessing==0 ) {
					
								opToken = LT(1);
							
				}
				op=mulOperator();
				result2=factor();
				if ( inputState.guessing==0 ) {
					
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
			}
			else {
				break _loop3215;
			}
			
		} while (true);
		}
		if ( inputState.guessing==0 ) {
			
						if(result.isConst)
							context.codeBlock().removeLastStatement();
					
		}
		return result;
	}
	
	public final Operation  addOperator() throws RecognitionException, TokenStreamException {
		Operation op = null;
		
		
		switch ( LA(1)) {
		case PLUS:
		case MINUS:
		{
			op=plusMinus();
			break;
		}
		case LITERAL_OR:
		{
			match(LITERAL_OR);
			if ( inputState.guessing==0 ) {
				op = Operation.OR;
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return op;
	}
	
	public final ExpressionResult  factor() throws RecognitionException, TokenStreamException {
		ExpressionResult result = null;
		
		Token  t1 = null;
		Token  t2 = null;
		Token  t3 = null;
		Token  t4 = null;
		Token  tildeToken = null;
		
					Token procToken = null;
					boolean isProc = false;
					Variable v = null;
					Type param0Type = null;
				
		
		switch ( LA(1)) {
		case INT:
		{
			t1 = LT(1);
			match(INT);
			if ( inputState.guessing==0 ) {
				
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
						
			}
			break;
		}
		case FLOAT:
		{
			t2 = LT(1);
			match(FLOAT);
			if ( inputState.guessing==0 ) {
				
							result = new ExpressionResult(true, new TypedValue(Type.RealType, Float.parseFloat(t2.getText().replace('d', 'e').replace('D','e'))));
						
			}
			break;
		}
		case CHAR:
		{
			t3 = LT(1);
			match(CHAR);
			if ( inputState.guessing==0 ) {
				
							result = new ExpressionResult(true, new TypedValue(Type.CharType, (char)Long.parseLong(t3.getText().replace("x", "").replace("X",""), 16)));
						
			}
			break;
		}
		case STRING:
		{
			t4 = LT(1);
			match(STRING);
			if ( inputState.guessing==0 ) {
				
							String tmp = t4.getText().substring(1, t4.getText().length() - 1);
							if(t4.getText().charAt(0) == '\"')
								tmp = tmp.replace("\"\"","\0").replace("\0","\"");
							else
								tmp = tmp.replace("\'\'","\0").replace("\0","\'");
							result = new ExpressionResult(true, new TypedValue(Type.StringType, tmp));
						
			}
			break;
		}
		case TRUE:
		{
			match(TRUE);
			if ( inputState.guessing==0 ) {
				
							result = new ExpressionResult(true, new TypedValue(Type.BooleanType, (Boolean)true));
						
			}
			break;
		}
		case FALSE:
		{
			match(FALSE);
			if ( inputState.guessing==0 ) {
				
							result = new ExpressionResult(true, new TypedValue(Type.BooleanType, (Boolean)false));
						
			}
			break;
		}
		case ODD:
		case EVEN:
		case ABS:
		{
			result=builtInExpr();
			break;
		}
		case IDENT:
		{
			procToken=qualident();
			if ( inputState.guessing==0 ) {
				
						v = context.variables.get(context.getVariableId(procToken.getText()));
					
			}
			{
			if (((_tokenSet_3.member(LA(1))) && (_tokenSet_4.member(LA(2))))&&(v.type.type == BuiltInType.PROCEDURE)) {
				param0Type=actualParameters(procToken);
				if ( inputState.guessing==0 ) {
					
							context.codeBlock().statements.add(AsmStatement.Call(v.id, procToken));
							if(!v.type.isInvoke())
								result = new ExpressionResult(false, new TypedValue(v.type.returnType()));
							else
								result = new ExpressionResult(false, new TypedValue(VirtualMachine.invokeReturnType(v, param0Type)));
						
				}
			}
			else if ((_tokenSet_6.member(LA(1))) && (_tokenSet_4.member(LA(2)))) {
				if ( inputState.guessing==0 ) {
					
							if(v.isConst && v.type.type.canBeConst())
								result = new ExpressionResult(true, v.value());
							else
							{
								result = new ExpressionResult(false, new TypedValue(v.type));
									context.codeBlock().statements.add(AsmStatement.PushVar(v.id, procToken));
							}
						
				}
				result=arrayIndex(v, result);
			}
			else {
				throw new NoViableAltException(LT(1), getFilename());
			}
			
			}
			break;
		}
		case LPAREN:
		{
			match(LPAREN);
			result=expression();
			match(RPAREN);
			break;
		}
		case TILDE:
		{
			tildeToken = LT(1);
			match(TILDE);
			result=factor();
			if ( inputState.guessing==0 ) {
				
						if(result.isConst)
								result.value = Operation.NOT.Perform(result.value);
							else {
								context.codeBlock().statements.add(AsmStatement.UnOp(Operation.NOT, tildeToken));
								result = new ExpressionResult(false, new TypedValue(Operation.NOT.returnType(result.value.type)));
							}
					
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return result;
	}
	
	public final Operation  mulOperator() throws RecognitionException, TokenStreamException {
		Operation op = null;
		
		
		switch ( LA(1)) {
		case STAR:
		{
			match(STAR);
			if ( inputState.guessing==0 ) {
				op = Operation.TIMES;
			}
			break;
		}
		case SLASH:
		{
			match(SLASH);
			if ( inputState.guessing==0 ) {
				op = Operation.DIVIDE;
			}
			break;
		}
		case LITERAL_DIV:
		{
			match(LITERAL_DIV);
			if ( inputState.guessing==0 ) {
				op = Operation.DIV;
			}
			break;
		}
		case LITERAL_MOD:
		{
			match(LITERAL_MOD);
			if ( inputState.guessing==0 ) {
				op = Operation.MOD;
			}
			break;
		}
		case AMP:
		{
			match(AMP);
			if ( inputState.guessing==0 ) {
				op = Operation.AND;
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return op;
	}
	
	public final Type  actualParamList(
		Type functionType, int pos
	) throws RecognitionException, TokenStreamException {
		Type param0Type = null;
		
		
		param0Type=actualParam(functionType, pos);
		{
		if (((LA(1)==COMMA))&&(functionType.isRefParam().size() > pos + 1)) {
			match(COMMA);
			actualParamList(functionType, pos + 1);
		}
		else if ((LA(1)==RPAREN)) {
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		}
		return param0Type;
	}
	
	public final Type  actualParam(
		Type functionType, int pos
	) throws RecognitionException, TokenStreamException {
		Type paramType = null;
		
		ExpressionResult r = null;
		
		if (((LA(1)==IDENT) && (_tokenSet_7.member(LA(2))))&&(functionType.isRefParam().get(pos))) {
			paramType=varParam();
			if ( inputState.guessing==0 ) {
				
							if(!functionType.isInvoke() && !paramType.equals(functionType.paramTypes().get(pos)))
								throw new SemanticException("Types of actual and formal var parameters must be identical");
						
			}
		}
		else if (((_tokenSet_2.member(LA(1))) && (_tokenSet_8.member(LA(2))))&&(!functionType.isRefParam().get(pos))) {
			r=nonConstExpression();
			if ( inputState.guessing==0 ) {
				
							if(!functionType.isInvoke() && !r.value.type.convertableTo(functionType.paramTypes().get(pos)))
								throw new SemanticException("Cannot convert \"" + r.value.type + "\" to \"" + functionType.paramTypes().get(pos) + "\"");
							paramType = r.value.type;
						
			}
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		return paramType;
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"INC",
		"DEC",
		"BREAK",
		"CONTINUE",
		"HALT",
		"ODD",
		"EVEN",
		"ABS",
		"TRUE",
		"FALSE",
		"COMMENT",
		"NEWLINE",
		"WHITESPACE",
		"STAR",
		"DOT",
		"EQUAL",
		"COMMA",
		"SEMICOLON",
		"LPAREN",
		"RPAREN",
		"ASSIGN",
		"COLON",
		"DOTDOT",
		"LBR",
		"RBR",
		"REF",
		"LBRACE",
		"RBRACE",
		"NEQUAL",
		"LEQ",
		"LESS",
		"GEQ",
		"GREATER",
		"PLUS",
		"MINUS",
		"SLASH",
		"AMP",
		"TILDE",
		"STROKE",
		"NUMERIC",
		"INT",
		"FLOAT",
		"CHAR",
		"STRING",
		"IDENT",
		"DIGIT",
		"HEXLETTER",
		"HEXDIGIT",
		"LETTER",
		"\"MODULE\"",
		"\"BEGIN\"",
		"\"END\"",
		"\"IMPORT\"",
		"\"CONST\"",
		"\"TYPE\"",
		"\"VAR\"",
		"\"PROCEDURE\"",
		"\"ARRAY\"",
		"\"OF\"",
		"\"EXIT\"",
		"\"RETURN\"",
		"\"IF\"",
		"\"THEN\"",
		"\"ELSIF\"",
		"\"ELSE\"",
		"\"CASE\"",
		"\"WHILE\"",
		"\"DO\"",
		"\"REPEAT\"",
		"\"UNTIL\"",
		"\"LOOP\"",
		"\"FOR\"",
		"\"TO\"",
		"\"BY\"",
		"\"OR\"",
		"\"DIV\"",
		"\"MOD\""
	};
	
	private static final long[] mk_tokenSet_0() {
		long[] data = { 4647717010610454528L, 127644L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_0 = new BitSet(mk_tokenSet_0());
	private static final long[] mk_tokenSet_1() {
		long[] data = { 36028797025517568L, 536L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_1 = new BitSet(mk_tokenSet_1());
	private static final long[] mk_tokenSet_2() {
		long[] data = { 547969111703040L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_2 = new BitSet(mk_tokenSet_2());
	private static final long[] mk_tokenSet_3() {
		long[] data = { 4647717010459459584L, 127644L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_3 = new BitSet(mk_tokenSet_3());
	private static final long[] mk_tokenSet_4() {
		long[] data = { -89522240592658448L, 131071L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_4 = new BitSet(mk_tokenSet_4());
	private static final long[] mk_tokenSet_5() {
		long[] data = { 1649267572736L, 98304L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_5 = new BitSet(mk_tokenSet_5());
	private static final long[] mk_tokenSet_6() {
		long[] data = { 4647717010589483008L, 127644L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_6 = new BitSet(mk_tokenSet_6());
	private static final long[] mk_tokenSet_7() {
		long[] data = { 143917056L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_7 = new BitSet(mk_tokenSet_7());
	private static final long[] mk_tokenSet_8() {
		long[] data = { 549751667703296L, 114688L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_8 = new BitSet(mk_tokenSet_8());
	
	}
