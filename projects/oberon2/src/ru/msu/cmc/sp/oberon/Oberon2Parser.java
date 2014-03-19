// $ANTLR : "oberon.g" -> "Oberon2Parser.java"$

package ru.msu.cmc.sp.oberon;
import java.util.ArrayList;

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

public class Oberon2Parser extends antlr.LLkParser       implements Oberon2ScannerTokenTypes
 {

protected Oberon2Parser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
}

public Oberon2Parser(TokenBuffer tokenBuf) {
  this(tokenBuf,1);
}

protected Oberon2Parser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
}

public Oberon2Parser(TokenStream lexer) {
  this(lexer,1);
}

public Oberon2Parser(ParserSharedInputState state) {
  super(state,1);
  tokenNames = _tokenNames;
}

	public final Token  qualident() throws RecognitionException, TokenStreamException {
		Token tok = null;
		
		Token  t1 = null;
		Token  t2 = null;
		
		{
		boolean synPredMatched224 = false;
		if (((LA(1)==IDENT))) {
			int _m224 = mark();
			synPredMatched224 = true;
			inputState.guessing++;
			try {
				{
				match(IDENT);
				match(DOT);
				}
			}
			catch (RecognitionException pe) {
				synPredMatched224 = false;
			}
			rewind(_m224);
inputState.guessing--;
		}
		if ( synPredMatched224 ) {
			t1 = LT(1);
			match(IDENT);
			match(DOT);
			if ( inputState.guessing==0 ) {
				tok = t1;
			}
		}
		else if ((LA(1)==IDENT)) {
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		}
		t2 = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			
					if(tok == null)
						return t2;
					else {
						tok.setText(tok.getText() + "." + t2.getText());
					}
				
		}
		return tok;
	}
	
	public final int  integer() throws RecognitionException, TokenStreamException {
		int l = 0;
		
		Expression.Constant e = null;
		
		e=constExpression();
		if ( inputState.guessing==0 ) {
			e = (Expression.Constant)Expression.TypeCast(Type.LongInt, e); l = (int)(long)(Long)e.evaluate();
		}
		return l;
	}
	
	public final Expression.Constant  constExpression() throws RecognitionException, TokenStreamException {
		Expression.Constant result = null;
		
		Expression e = null;
		
		e=expression();
		if ( inputState.guessing==0 ) {
			
					if(!(e instanceof Expression.Constant))
						throw new Error(Error.COMPILEERROR, "Constant expression expected");
					result = (Expression.Constant)e;
				
		}
		return result;
	}
	
	public final void module() throws RecognitionException, TokenStreamException {
		
		Token  mTok = null;
		Token  mTok2 = null;
		Identifier main = null; Statement mainBody = null;
		
		match(LITERAL_MODULE);
		mTok = LT(1);
		match(IDENT);
		match(SEMICOLON);
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
			
					Module.addIdent(main = new Identifier(Type.VoidFunction, null), mTok);
					Module.enterFunction(main);
				
		}
		declarationSequence();
		{
		switch ( LA(1)) {
		case LITERAL_BEGIN:
		{
			match(LITERAL_BEGIN);
			mainBody=statementSequence();
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
		match(LITERAL_END);
		mTok2 = LT(1);
		match(IDENT);
		match(DOT);
		if ( inputState.guessing==0 ) {
			
				if(!mTok.getText().equals(mTok2.getText()))
				    	throw new Error(Error.COMPILEERROR, "Expected \"" + mTok.getText() + "\" instead of \"" + mTok2.getText() + "\"");
				main.setValue(new FunctionValue(Module.leaveFunction(), mainBody));
				Module.main = Statement.ProcedureCall(main.getNo(), new ArrayList<Expression>());
			
		}
	}
	
	public final void importList() throws RecognitionException, TokenStreamException {
		
		Token  t1 = null;
		Token  t2 = null;
		
		match(LITERAL_IMPORT);
		t1 = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			Module.Import(t1.getText());
		}
		{
		_loop231:
		do {
			if ((LA(1)==COMMA)) {
				match(COMMA);
				t2 = LT(1);
				match(IDENT);
				if ( inputState.guessing==0 ) {
					Module.Import(t2.getText());
				}
			}
			else {
				break _loop231;
			}
			
		} while (true);
		}
		match(SEMICOLON);
	}
	
	public final void declarationSequence() throws RecognitionException, TokenStreamException {
		
		
		{
		_loop247:
		do {
			switch ( LA(1)) {
			case LITERAL_CONST:
			{
				match(LITERAL_CONST);
				{
				_loop241:
				do {
					if ((LA(1)==IDENT)) {
						constantDeclaration();
						match(SEMICOLON);
					}
					else {
						break _loop241;
					}
					
				} while (true);
				}
				break;
			}
			case LITERAL_TYPE:
			{
				match(LITERAL_TYPE);
				{
				_loop243:
				do {
					if ((LA(1)==IDENT)) {
						typeDeclaration();
						match(SEMICOLON);
					}
					else {
						break _loop243;
					}
					
				} while (true);
				}
				break;
			}
			case LITERAL_VAR:
			{
				match(LITERAL_VAR);
				{
				_loop245:
				do {
					if ((LA(1)==IDENT)) {
						variableDeclaration();
						match(SEMICOLON);
					}
					else {
						break _loop245;
					}
					
				} while (true);
				}
				break;
			}
			case LITERAL_PROCEDURE:
			{
				match(LITERAL_PROCEDURE);
				{
				switch ( LA(1)) {
				case REF:
				{
					forwardDeclaration();
					match(SEMICOLON);
					break;
				}
				case IDENT:
				{
					procedureDeclaration();
					match(SEMICOLON);
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
			default:
			{
				break _loop247;
			}
			}
		} while (true);
		}
	}
	
	public final Statement.Sequence  statementSequence() throws RecognitionException, TokenStreamException {
		Statement.Sequence s = Statement.Sequence();
		
		Statement s1 = null, s2 = null;
		
		s1=statement();
		if ( inputState.guessing==0 ) {
			s.statements.add(s1);
		}
		{
		_loop306:
		do {
			if ((LA(1)==SEMICOLON)) {
				match(SEMICOLON);
				s2=statement();
				if ( inputState.guessing==0 ) {
					s.statements.add(s2);
				}
			}
			else {
				break _loop306;
			}
			
		} while (true);
		}
		return s;
	}
	
	public final Type  formalType() throws RecognitionException, TokenStreamException {
		Type t = null;
		
		
		switch ( LA(1)) {
		case IDENT:
		{
			t=typeDef();
			break;
		}
		case LITERAL_ARRAY:
		{
			t=formalArrayType();
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return t;
	}
	
	public final Type  typeDef() throws RecognitionException, TokenStreamException {
		Type tt = null;
		
		Token t = null;
		
		t=qualident();
		if ( inputState.guessing==0 ) {
			
					Identifier id = Module.idents.get(Module.getIdentNo(t));
					if(id.getType().typeId() != Type.TYPE)
						throw new Error(Error.COMPILEERROR, "Type expected");
					tt = (Type)id.getValue();
				
		}
		return tt;
	}
	
	public final Type  formalArrayType() throws RecognitionException, TokenStreamException {
		Type tt = null;
		
		Type t = null;
		
		match(LITERAL_ARRAY);
		match(LITERAL_OF);
		t=nonArrayType();
		if ( inputState.guessing==0 ) {
			tt = Type.Array(0, t);
		}
		return tt;
	}
	
	public final Type  type() throws RecognitionException, TokenStreamException {
		Type t = null;
		
		
		switch ( LA(1)) {
		case IDENT:
		{
			t=typeDef();
			break;
		}
		case LITERAL_ARRAY:
		{
			t=arrayType();
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return t;
	}
	
	public final Type  arrayType() throws RecognitionException, TokenStreamException {
		Type tt = null;
		
		int l = 0; Type t = null;
		
		match(LITERAL_ARRAY);
		l=integer();
		match(LITERAL_OF);
		t=type();
		if ( inputState.guessing==0 ) {
			tt = Type.Array(l, t);
		}
		return tt;
	}
	
	public final Type  nonArrayType() throws RecognitionException, TokenStreamException {
		Type tt = null;
		
		Token t = null;
		
		t=qualident();
		if ( inputState.guessing==0 ) {
			
					Identifier id = Module.idents.get(Module.getIdentNo(t));
					if(id.getType().typeId() != Type.TYPE)
						throw new Error(Error.COMPILEERROR, "Type expected");
					if(((Type)id.getValue()).typeId() == Type.ARRAY)
						throw new Error(Error.COMPILEERROR, "2-dimensional arrays are not supported");
					tt = (Type)id.getValue();
				
		}
		return tt;
	}
	
	public final void constantDeclaration() throws RecognitionException, TokenStreamException {
		
		Token  t = null;
		Expression.Constant e = null;
		
		t = LT(1);
		match(IDENT);
		match(EQUAL);
		e=constExpression();
		if ( inputState.guessing==0 ) {
			Module.addIdent(new Identifier(e.returnType(), e.evaluate()), t);
		}
	}
	
	public final void typeDeclaration() throws RecognitionException, TokenStreamException {
		
		Token  t = null;
		Type tt = null;
		
		t = LT(1);
		match(IDENT);
		match(EQUAL);
		tt=type();
		if ( inputState.guessing==0 ) {
			Module.addIdent(new Identifier(Type.Type, tt), t);
		}
	}
	
	public final void variableDeclaration() throws RecognitionException, TokenStreamException {
		
		ArrayList<Token> ts = null; Type t = null;
		
		ts=identList();
		match(COLON);
		t=type();
		if ( inputState.guessing==0 ) {
			for(int i = 0; i < ts.size(); i++) Module.addIdent(new Identifier(t), ts.get(i));
		}
	}
	
	public final void forwardDeclaration() throws RecognitionException, TokenStreamException {
		
		Token  t = null;
		Type tt = null;
		
		match(REF);
		t = LT(1);
		match(IDENT);
		tt=formalParameters(false);
		if ( inputState.guessing==0 ) {
			Module.addIdent(new Identifier(tt, null), t);
		}
	}
	
	public final void procedureDeclaration() throws RecognitionException, TokenStreamException {
		
		Token  t = null;
		Token  t2 = null;
		Type tt = null; Identifier funcIdent = null; Statement body = null;
		
		t = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			
					Module.addIdent(funcIdent = new Identifier(Type.VoidFunction, null), t);
					Module.enterFunction(funcIdent.getNo());
				
		}
		tt=formalParameters(true);
		if ( inputState.guessing==0 ) {
			
					funcIdent.setType(tt);
				
		}
		match(SEMICOLON);
		body=procedureBody();
		t2 = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			
					if(!t.getText().equals(t2.getText())) 
						throw new Error(Error.COMPILEERROR, "Expected \"" + t.getText() + "\" instead of \"" + t2.getText() + "\"");
					funcIdent.setValue(new FunctionValue(Module.leaveFunction(), body));
					Module.checkForwardDecl(funcIdent);
				
		}
	}
	
	public final ArrayList<Token>  identList() throws RecognitionException, TokenStreamException {
		ArrayList<Token> ts = new ArrayList<Token>();
		
		Token  t = null;
		Token  t2 = null;
		
		t = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			ts.add(t);
		}
		{
		_loop252:
		do {
			if ((LA(1)==COMMA)) {
				match(COMMA);
				t2 = LT(1);
				match(IDENT);
				if ( inputState.guessing==0 ) {
					ts.add(t2);
				}
			}
			else {
				break _loop252;
			}
			
		} while (true);
		}
		return ts;
	}
	
	public final Type  formalParameters(
		boolean addVars
	) throws RecognitionException, TokenStreamException {
		Type t = null;
		
		
				Type retType = Type.Void;
				ArrayList<Type> paramTypes = new ArrayList<Type>();
				ArrayList<Boolean> varParam = new ArrayList<Boolean>();
			
		
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
				fPSection(paramTypes, varParam, addVars);
				{
				_loop262:
				do {
					if ((LA(1)==SEMICOLON)) {
						match(SEMICOLON);
						fPSection(paramTypes, varParam, addVars);
					}
					else {
						break _loop262;
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
			retType=nonArrayType();
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
			t = Type.Function(paramTypes.size(), paramTypes, varParam, retType);
		}
		return t;
	}
	
	public final Statement  procedureBody() throws RecognitionException, TokenStreamException {
		Statement body = Statement.Nop();
		
		
		declarationSequence();
		{
		switch ( LA(1)) {
		case LITERAL_BEGIN:
		{
			match(LITERAL_BEGIN);
			body=statementSequence();
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
		match(LITERAL_END);
		return body;
	}
	
	public final void fPSection(
		ArrayList<Type> paramTypes, ArrayList<Boolean> varParam, boolean addVars
	) throws RecognitionException, TokenStreamException {
		
		Token  t = null;
		Token  t2 = null;
		
				boolean isVarParam = false;
				ArrayList<Token> toks = new ArrayList<Token>();
				Type type = null;
			
		
		{
		switch ( LA(1)) {
		case LITERAL_VAR:
		{
			match(LITERAL_VAR);
			if ( inputState.guessing==0 ) {
				isVarParam = true;
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
		t = LT(1);
		match(IDENT);
		if ( inputState.guessing==0 ) {
			toks.add(t);
		}
		{
		_loop267:
		do {
			if ((LA(1)==COMMA)) {
				match(COMMA);
				t2 = LT(1);
				match(IDENT);
				if ( inputState.guessing==0 ) {
					toks.add(t2);
				}
			}
			else {
				break _loop267;
			}
			
		} while (true);
		}
		match(COLON);
		type=formalType();
		if ( inputState.guessing==0 ) {
			
					for(int i = 0; i < toks.size(); i++) {
						varParam.add(isVarParam);
						paramTypes.add(type);
						if(addVars)
							Module.addIdent(new Identifier(type), toks.get(i));
					}
				
		}
	}
	
	public final Expression  expression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		Expression e1 = null; int opCode = 0;
		
		e=simpleExpression();
		{
		switch ( LA(1)) {
		case EQUAL:
		case NEQUAL:
		case LESS:
		case LEQ:
		case GREATER:
		case GEQ:
		{
			opCode=relation();
			e1=simpleExpression();
			if ( inputState.guessing==0 ) {
				e = Expression.Operation(opCode, e, e1);
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
		return e;
	}
	
	public final Expression  simpleExpression() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		int opCode = 0; Expression e1 = null;
		
		{
		switch ( LA(1)) {
		case PLUS:
		case MINUS:
		{
			{
			opCode=unaryAddOperator();
			e=term();
			if ( inputState.guessing==0 ) {
				if(opCode == Expression.MINUS) e = Expression.Operation(opCode, e);
			}
			}
			break;
		}
		case INTEGER:
		case REAL:
		case CHAR:
		case LPAREN:
		case TILDE:
		case IDENT:
		case STRING:
		case LITERAL_ABS:
		case LITERAL_ASH:
		case LITERAL_LSH:
		case LITERAL_CHR:
		case LITERAL_ORD:
		case LITERAL_ENTIER:
		case LITERAL_EVEN:
		case LITERAL_ODD:
		case LITERAL_LONG:
		case LITERAL_SHORT:
		case LITERAL_LEN:
		{
			e=term();
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		{
		_loop275:
		do {
			if ((LA(1)==PLUS||LA(1)==MINUS||LA(1)==LITERAL_OR)) {
				opCode=addOperator();
				e1=term();
				if ( inputState.guessing==0 ) {
					e = Expression.Operation(opCode, e, e1);
				}
			}
			else {
				break _loop275;
			}
			
		} while (true);
		}
		return e;
	}
	
	public final int  relation() throws RecognitionException, TokenStreamException {
		int opCode = 0;
		
		
		switch ( LA(1)) {
		case EQUAL:
		{
			match(EQUAL);
			if ( inputState.guessing==0 ) {
				opCode = Expression.EQUAL;
			}
			break;
		}
		case NEQUAL:
		{
			match(NEQUAL);
			if ( inputState.guessing==0 ) {
				opCode = Expression.NOTEQUAL;
			}
			break;
		}
		case LESS:
		{
			match(LESS);
			if ( inputState.guessing==0 ) {
				opCode = Expression.LESS;
			}
			break;
		}
		case LEQ:
		{
			match(LEQ);
			if ( inputState.guessing==0 ) {
				opCode = Expression.LESSEQUAL;
			}
			break;
		}
		case GREATER:
		{
			match(GREATER);
			if ( inputState.guessing==0 ) {
				opCode = Expression.GREATER;
			}
			break;
		}
		case GEQ:
		{
			match(GEQ);
			if ( inputState.guessing==0 ) {
				opCode = Expression.GREATEREQUAL;
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return opCode;
	}
	
	public final int  unaryAddOperator() throws RecognitionException, TokenStreamException {
		int opCode = 0;
		
		
		switch ( LA(1)) {
		case PLUS:
		{
			match(PLUS);
			if ( inputState.guessing==0 ) {
				opCode = Expression.PLUS;
			}
			break;
		}
		case MINUS:
		{
			match(MINUS);
			if ( inputState.guessing==0 ) {
				opCode = Expression.MINUS;
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return opCode;
	}
	
	public final Expression  term() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		int opCode = 0; Expression e1 = null;
		
		e=factor();
		{
		_loop278:
		do {
			if ((_tokenSet_0.member(LA(1)))) {
				opCode=mulOperator();
				e1=factor();
				if ( inputState.guessing==0 ) {
					e = Expression.Operation(opCode, e, e1);
				}
			}
			else {
				break _loop278;
			}
			
		} while (true);
		}
		return e;
	}
	
	public final int  addOperator() throws RecognitionException, TokenStreamException {
		int opCode = 0;
		
		
		switch ( LA(1)) {
		case PLUS:
		case MINUS:
		{
			opCode=unaryAddOperator();
			break;
		}
		case LITERAL_OR:
		{
			match(LITERAL_OR);
			if ( inputState.guessing==0 ) {
				opCode = Expression.OR;
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return opCode;
	}
	
	public final Expression  factor() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		Token  t1 = null;
		Token  t2 = null;
		Token  t3 = null;
		Token  t4 = null;
		Token t = null; Identifier v = null; Expression e1 = null;
		
		switch ( LA(1)) {
		case INTEGER:
		{
			t1 = LT(1);
			match(INTEGER);
			if ( inputState.guessing==0 ) {
				
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
					
			}
			break;
		}
		case REAL:
		{
			t2 = LT(1);
			match(REAL);
			if ( inputState.guessing==0 ) {
				e = Expression.Constant(Type.Real, Float.parseFloat(t2.getText()));
			}
			break;
		}
		case CHAR:
		{
			t3 = LT(1);
			match(CHAR);
			if ( inputState.guessing==0 ) {
				
						long value = Long.parseLong(t3.getText(), 16);
						if(value < 0 || value > 255)
							throw new Error(Error.DOMAINERROR);
						e = Expression.Constant(Type.Char, (char)value);
					
			}
			break;
		}
		case STRING:
		{
			t4 = LT(1);
			match(STRING);
			if ( inputState.guessing==0 ) {
				e = Expression.Constant(Type.String, t4.getText());
			}
			break;
		}
		case LPAREN:
		{
			match(LPAREN);
			e=expression();
			match(RPAREN);
			break;
		}
		case TILDE:
		{
			match(TILDE);
			e=factor();
			if ( inputState.guessing==0 ) {
				e = Expression.Operation(Expression.NOT, e);
			}
			break;
		}
		case IDENT:
		{
			t=qualident();
			if ( inputState.guessing==0 ) {
				v = Module.idents.get(Module.getIdentNo(t));
			}
			{
			if (((_tokenSet_1.member(LA(1))))&&(v.getType().typeId() == Type.FUNCTION)) {
				e=actualParameters(v);
			}
			else if ((_tokenSet_2.member(LA(1)))) {
				{
				switch ( LA(1)) {
				case LBR:
				{
					match(LBR);
					e1=expression();
					match(RBR);
					break;
				}
				case STAR:
				case EQUAL:
				case COMMA:
				case SEMICOLON:
				case RPAREN:
				case RBR:
				case NEQUAL:
				case LESS:
				case LEQ:
				case GREATER:
				case GEQ:
				case PLUS:
				case MINUS:
				case SLASH:
				case AMP:
				case LITERAL_END:
				case LITERAL_OF:
				case LITERAL_OR:
				case LITERAL_DIV:
				case LITERAL_MOD:
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
					e = Expression.Variable(v.getNo(), e1);
				}
			}
			else {
				throw new NoViableAltException(LT(1), getFilename());
			}
			
			}
			break;
		}
		case LITERAL_ABS:
		{
			match(LITERAL_ABS);
			match(LPAREN);
			e=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Abs(e);
			}
			break;
		}
		case LITERAL_ASH:
		{
			match(LITERAL_ASH);
			match(LPAREN);
			e=expression();
			match(COMMA);
			e1=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Ash(e, e1);
			}
			break;
		}
		case LITERAL_LSH:
		{
			match(LITERAL_LSH);
			match(LPAREN);
			e=expression();
			match(COMMA);
			e1=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Lsh(e, e1);
			}
			break;
		}
		case LITERAL_CHR:
		{
			match(LITERAL_CHR);
			match(LPAREN);
			e=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Chr(e);
			}
			break;
		}
		case LITERAL_ORD:
		{
			match(LITERAL_ORD);
			match(LPAREN);
			e=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Ord(e);
			}
			break;
		}
		case LITERAL_ENTIER:
		{
			match(LITERAL_ENTIER);
			match(LPAREN);
			e=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Entier(e);
			}
			break;
		}
		case LITERAL_EVEN:
		{
			match(LITERAL_EVEN);
			match(LPAREN);
			e=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Even(e);
			}
			break;
		}
		case LITERAL_ODD:
		{
			match(LITERAL_ODD);
			match(LPAREN);
			e=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Odd(e);
			}
			break;
		}
		case LITERAL_LONG:
		{
			match(LITERAL_LONG);
			match(LPAREN);
			e=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Long(e);
			}
			break;
		}
		case LITERAL_SHORT:
		{
			match(LITERAL_SHORT);
			match(LPAREN);
			e=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Short(e);
			}
			break;
		}
		case LITERAL_LEN:
		{
			match(LITERAL_LEN);
			match(LPAREN);
			e=expression();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				e = Expression.Len(e);
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return e;
	}
	
	public final int  mulOperator() throws RecognitionException, TokenStreamException {
		int opCode = 0;
		
		
		switch ( LA(1)) {
		case STAR:
		{
			match(STAR);
			if ( inputState.guessing==0 ) {
				opCode = Expression.TIMES;
			}
			break;
		}
		case SLASH:
		{
			match(SLASH);
			if ( inputState.guessing==0 ) {
				opCode = Expression.DIVIDE;
			}
			break;
		}
		case LITERAL_DIV:
		{
			match(LITERAL_DIV);
			if ( inputState.guessing==0 ) {
				opCode = Expression.DIV;
			}
			break;
		}
		case LITERAL_MOD:
		{
			match(LITERAL_MOD);
			if ( inputState.guessing==0 ) {
				opCode = Expression.MOD;
			}
			break;
		}
		case AMP:
		{
			match(AMP);
			if ( inputState.guessing==0 ) {
				opCode = Expression.AND;
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return opCode;
	}
	
	public final Expression.FunctionCall  actualParameters(
		Identifier funcIdent
	) throws RecognitionException, TokenStreamException {
		Expression.FunctionCall e = null;
		
		ArrayList<Expression> args = new ArrayList<Expression>();
		
		{
		if (((LA(1)==LPAREN))&&(funcIdent.getType().funcParamCount() > 0)) {
			match(LPAREN);
			args=actualParameters2(funcIdent, 0);
			match(RPAREN);
		}
		else if ((LA(1)==LPAREN)) {
			match(LPAREN);
			match(RPAREN);
		}
		else if ((_tokenSet_3.member(LA(1)))) {
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		}
		if ( inputState.guessing==0 ) {
			e = Expression.FunctionCall(funcIdent.getNo(), args);
		}
		return e;
	}
	
	public final ArrayList<Expression>  actualParameters2(
		Identifier funcIdent, int pos
	) throws RecognitionException, TokenStreamException {
		ArrayList<Expression> args = null;
		
		Expression e = null;
		
		{
		if (((LA(1)==IDENT))&&(funcIdent.getType().funcIsVarParam(pos))) {
			e=designatorExpr();
		}
		else if ((_tokenSet_4.member(LA(1)))) {
			e=expression();
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		}
		{
		if (((LA(1)==COMMA))&&(pos < funcIdent.getType().funcParamCount())) {
			match(COMMA);
			args=actualParameters2(funcIdent, pos + 1);
		}
		else if ((LA(1)==RPAREN)) {
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		}
		if ( inputState.guessing==0 ) {
			
					if(args != null)
						args.add(0, e);
					else {
						args = new ArrayList<Expression>();
						args.add(e);
					}
				
		}
		return args;
	}
	
	public final Expression  designatorExpr() throws RecognitionException, TokenStreamException {
		Expression e = null;
		
		Token t = null; Expression e1 = null;
		
		t=qualident();
		{
		switch ( LA(1)) {
		case LBR:
		{
			match(LBR);
			e1=expression();
			match(RBR);
			break;
		}
		case COMMA:
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
		if ( inputState.guessing==0 ) {
			
					if(Module.idents.get(Module.getIdentNo(t)).isConst())
						throw new Error(Error.COMPILEERROR, "Cannot pass constant as var parameter");
					e = Expression.Variable(Module.getIdentNo(t), e1);
				
		}
		return e;
	}
	
	public final Statement  statement() throws RecognitionException, TokenStreamException {
		Statement s = Statement.Nop();
		
		
		{
		switch ( LA(1)) {
		case LITERAL_FOR:
		{
			s=forStatement();
			break;
		}
		case LITERAL_IF:
		{
			s=ifStatement();
			break;
		}
		case LITERAL_WHILE:
		{
			s=whileStatement();
			break;
		}
		case LITERAL_REPEAT:
		{
			s=repeatStatement();
			break;
		}
		case LITERAL_LOOP:
		{
			s=loopStatement();
			break;
		}
		case LITERAL_EXIT:
		{
			s=exitStatement();
			break;
		}
		case LITERAL_RETURN:
		{
			s=returnStatement();
			break;
		}
		case LITERAL_INC:
		case LITERAL_DEC:
		case LITERAL_HALT:
		{
			s=defaultProc();
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
			boolean synPredMatched297 = false;
			if (((LA(1)==IDENT))) {
				int _m297 = mark();
				synPredMatched297 = true;
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
					synPredMatched297 = false;
				}
				rewind(_m297);
inputState.guessing--;
			}
			if ( synPredMatched297 ) {
				s=assignment();
			}
			else if ((LA(1)==IDENT)) {
				s=procedureCall();
			}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		return s;
	}
	
	public final Statement  assignment() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		Expression index = null, value = null; Token t = null; int varNo = 0;
		
		t=qualident();
		if ( inputState.guessing==0 ) {
			varNo = Module.getIdentNo(t);
		}
		{
		switch ( LA(1)) {
		case LBR:
		{
			match(LBR);
			index=expression();
			match(RBR);
			break;
		}
		case ASSIGN:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		match(ASSIGN);
		value=expression();
		if ( inputState.guessing==0 ) {
			s = Statement.Assignment(varNo, index, value);
		}
		return s;
	}
	
	public final Statement  procedureCall() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		Token t = null; Identifier funcIdent = null; Expression.FunctionCall call = null;
		
		t=qualident();
		if ( inputState.guessing==0 ) {
			funcIdent = Module.idents.get(Module.getIdentNo(t));
		}
		call=actualParameters(funcIdent);
		if ( inputState.guessing==0 ) {
			s = Statement.ProcedureCall(call);
		}
		return s;
	}
	
	public final Statement  forStatement() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		
				Token t = null; 
				int varNo = 0; 
				Expression from = null, to = null, by = Expression.Constant(Type.ShortInt, (Short)(short)1);
				Statement block = null;
			
		
		match(LITERAL_FOR);
		t=qualident();
		if ( inputState.guessing==0 ) {
			varNo = Module.getIdentNo(t);
		}
		match(ASSIGN);
		from=expression();
		match(LITERAL_TO);
		to=expression();
		{
		switch ( LA(1)) {
		case LITERAL_BY:
		{
			match(LITERAL_BY);
			by=constExpression();
			break;
		}
		case LITERAL_DO:
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
			Statement.For(varNo, from, to, by, null);
		}
		match(LITERAL_DO);
		block=statementSequence();
		match(LITERAL_END);
		if ( inputState.guessing==0 ) {
			s = Statement.For(varNo, from, to, by, block);
		}
		return s;
	}
	
	public final Statement  ifStatement() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		
		match(LITERAL_IF);
		s=ifWithoutIf();
		match(LITERAL_END);
		return s;
	}
	
	public final Statement  whileStatement() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		Statement.Sequence seq = Statement.Sequence(); Expression expr = null;
		
		match(LITERAL_WHILE);
		expr=expression();
		if ( inputState.guessing==0 ) {
			Statement.While(null, expr);
		}
		match(LITERAL_DO);
		seq=statementSequence();
		match(LITERAL_END);
		if ( inputState.guessing==0 ) {
			s = Statement.While(seq, expr);
		}
		return s;
	}
	
	public final Statement  repeatStatement() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		Statement.Sequence seq = Statement.Sequence(); Expression expr = null;
		
		match(LITERAL_REPEAT);
		seq=statementSequence();
		match(LITERAL_UNTIL);
		expr=expression();
		if ( inputState.guessing==0 ) {
			s = Statement.Repeat(seq, expr);
		}
		return s;
	}
	
	public final Statement  loopStatement() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		Statement block = null;
		
		match(LITERAL_LOOP);
		block=statementSequence();
		match(LITERAL_END);
		if ( inputState.guessing==0 ) {
			s = Statement.Loop(block);
		}
		return s;
	}
	
	public final Statement  exitStatement() throws RecognitionException, TokenStreamException {
		Statement s = Statement.Break();
		
		
		match(LITERAL_EXIT);
		return s;
	}
	
	public final Statement  returnStatement() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		Expression e = null;
		
		match(LITERAL_RETURN);
		{
		switch ( LA(1)) {
		case INTEGER:
		case REAL:
		case CHAR:
		case LPAREN:
		case PLUS:
		case MINUS:
		case TILDE:
		case IDENT:
		case STRING:
		case LITERAL_ABS:
		case LITERAL_ASH:
		case LITERAL_LSH:
		case LITERAL_CHR:
		case LITERAL_ORD:
		case LITERAL_ENTIER:
		case LITERAL_EVEN:
		case LITERAL_ODD:
		case LITERAL_LONG:
		case LITERAL_SHORT:
		case LITERAL_LEN:
		{
			e=expression();
			if ( inputState.guessing==0 ) {
				s = Statement.Return(e);
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
				s = Statement.Return(Expression.Constant(Type.Void, null));
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		return s;
	}
	
	public final Statement  defaultProc() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		Token t = null; int varNo = 0, code = 0; Expression index = null, expr = null;
		
		switch ( LA(1)) {
		case LITERAL_INC:
		{
			match(LITERAL_INC);
			match(LPAREN);
			t=qualident();
			if ( inputState.guessing==0 ) {
				varNo = Module.getIdentNo(t);
			}
			{
			switch ( LA(1)) {
			case LBR:
			{
				match(LBR);
				index=expression();
				match(RBR);
				break;
			}
			case COMMA:
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
			{
			switch ( LA(1)) {
			case COMMA:
			{
				match(COMMA);
				expr=expression();
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
			if ( inputState.guessing==0 ) {
				s = Statement.Inc(varNo, index, expr);
			}
			break;
		}
		case LITERAL_DEC:
		{
			match(LITERAL_DEC);
			match(LPAREN);
			t=qualident();
			if ( inputState.guessing==0 ) {
				varNo = Module.getIdentNo(t);
			}
			{
			switch ( LA(1)) {
			case LBR:
			{
				match(LBR);
				index=expression();
				match(RBR);
				break;
			}
			case COMMA:
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
			{
			switch ( LA(1)) {
			case COMMA:
			{
				match(COMMA);
				expr=expression();
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
			if ( inputState.guessing==0 ) {
				s = Statement.Dec(varNo, index, expr);
			}
			break;
		}
		case LITERAL_HALT:
		{
			match(LITERAL_HALT);
			match(LPAREN);
			code=integer();
			match(RPAREN);
			if ( inputState.guessing==0 ) {
				s = Statement.Halt(code);
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return s;
	}
	
	public final Statement  ifWithoutIf() throws RecognitionException, TokenStreamException {
		Statement s = null;
		
		Statement thenBlock = null, elseBlock = null; Expression expr = null;
		
		expr=expression();
		if ( inputState.guessing==0 ) {
			Statement.If(expr, null, null);
		}
		match(LITERAL_THEN);
		thenBlock=statementSequence();
		{
		switch ( LA(1)) {
		case LITERAL_ELSIF:
		{
			match(LITERAL_ELSIF);
			elseBlock=ifWithoutIf();
			break;
		}
		case LITERAL_ELSE:
		{
			match(LITERAL_ELSE);
			elseBlock=statementSequence();
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
			s = Statement.If(expr, thenBlock, elseBlock);
		}
		return s;
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"INTEGER",
		"REAL",
		"CHAR",
		"STAR",
		"DOT",
		"EQUAL",
		"COMMA",
		"SEMICOLON",
		"LPAREN",
		"RPAREN",
		"COLON",
		"DOTDOT",
		"LBR",
		"RBR",
		"REF",
		"LBRACE",
		"RBRACE",
		"NEQUAL",
		"LESS",
		"LEQ",
		"GREATER",
		"GEQ",
		"PLUS",
		"MINUS",
		"SLASH",
		"AMP",
		"TILDE",
		"STROKE",
		"ASSIGN",
		"COMMENT",
		"NEWLINE",
		"SPACE",
		"NUMBER",
		"IDENT",
		"STRING",
		"DIGIT",
		"HEX",
		"LETTER",
		"\"MODULE\"",
		"\"BEGIN\"",
		"\"END\"",
		"\"IMPORT\"",
		"\"ARRAY\"",
		"\"OF\"",
		"\"CONST\"",
		"\"TYPE\"",
		"\"VAR\"",
		"\"PROCEDURE\"",
		"\"ABS\"",
		"\"ASH\"",
		"\"LSH\"",
		"\"CHR\"",
		"\"ORD\"",
		"\"ENTIER\"",
		"\"EVEN\"",
		"\"ODD\"",
		"\"LONG\"",
		"\"SHORT\"",
		"\"LEN\"",
		"\"OR\"",
		"\"DIV\"",
		"\"MOD\"",
		"\"EXIT\"",
		"\"RETURN\"",
		"\"IF\"",
		"\"THEN\"",
		"\"ELSIF\"",
		"\"ELSE\"",
		"\"WHILE\"",
		"\"DO\"",
		"\"REPEAT\"",
		"\"UNTIL\"",
		"\"LOOP\"",
		"\"FOR\"",
		"\"TO\"",
		"\"BY\"",
		"\"INC\"",
		"\"DEC\"",
		"\"HALT\""
	};
	
	private static final long[] mk_tokenSet_0() {
		long[] data = { 805306496L, 3L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_0 = new BitSet(mk_tokenSet_0());
	private static final long[] mk_tokenSet_1() {
		long[] data = { -9223213706108584320L, 51939L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_1 = new BitSet(mk_tokenSet_1());
	private static final long[] mk_tokenSet_2() {
		long[] data = { -9223213706108522880L, 51939L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_2 = new BitSet(mk_tokenSet_2());
	private static final long[] mk_tokenSet_3() {
		long[] data = { -9223213706108588416L, 51939L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_3 = new BitSet(mk_tokenSet_3());
	private static final long[] mk_tokenSet_4() {
		long[] data = { 9218868850819338352L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_4 = new BitSet(mk_tokenSet_4());
	
	}
