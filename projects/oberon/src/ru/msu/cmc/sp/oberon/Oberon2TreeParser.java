// $ANTLR : "oberon.g" -> "Oberon2TreeParser.java"$

package ru.msu.cmc.sp.oberon;
import java.util.*;
import antlr.SemanticException;

import antlr.TreeParser;
import antlr.Token;
import antlr.collections.AST;
import antlr.RecognitionException;
import antlr.ANTLRException;
import antlr.NoViableAltException;
import antlr.MismatchedTokenException;
import antlr.SemanticException;
import antlr.collections.impl.BitSet;
import antlr.ASTPair;
import antlr.collections.impl.ASTArray;


public class Oberon2TreeParser extends antlr.TreeParser       implements Oberon2ScannerTokenTypes
 {

	public void ErrorOut1(String name)
	{
		System.out.println(name);
	}
	Oberon structure = new Oberon();

public Oberon2TreeParser() {
	tokenNames = _tokenNames;
}

	public final void module(AST _t) throws RecognitionException, SemanticException {
		
		AST module_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		AST ID1 = null;
		AST ID2 = null;
		
		try {      // for error handling
			if ( inputState.guessing==0 ) {
				
					structure.types.add("BOOLEAN");
					structure.types.add("CHAR");
					structure.types.add("SHORTINT");
					structure.types.add("LONGINT");
					structure.types.add("INTEGER");
					structure.types.add("REAL");
					structure.types.add("LONGREAL");
					
					//----- from http://unicorn.cmc.msu.ru/6sem/statement.shtml -------------
					//ABS
					structure.addStructure("proc", "ABS", "SHORTINT", 0, 0, 'P', "SHORTINT", false, 0);
					structure.addStructure("proc", "ABS", "INTEGER", 0, 0, 'P', "INTEGER", false, 0);
					structure.addStructure("proc", "ABS", "LONGINT", 0, 0, 'P', "LONGINT", false, 0);
					structure.addStructure("proc", "ABS", "REAL", 0, 0, 'P', "REAL", false, 0);
					structure.addStructure("proc", "ABS", "LONGREAL", 0, 0, 'P', "LONGREAL", false, 0);
				
					//ASH
					structure.addStructure("proc", "ASH", "LONGINT", 0, 0, 'P', "SHORTINT SHORTINT", false, 0);
					structure.addStructure("proc", "ASH", "LONGINT", 0, 0, 'P', "INTEGER INTEGER", false, 0);
					structure.addStructure("proc", "ASH", "LONGINT", 0, 0, 'P', "LONGINT LONGINT", false, 0);
				
					//CHR
					structure.addStructure("proc", "CHR", "CHAR", 0, 0, 'P', "SHORTINT", false, 0);
					structure.addStructure("proc", "CHR", "CHAR", 0, 0, 'P', "INTEGER", false, 0);
					structure.addStructure("proc", "CHR", "CHAR", 0, 0, 'P', "LONGINT", false, 0);
					
					//ENTIER
					structure.addStructure("proc", "ENTIER", "LONGINT", 0, 0, 'P', "REAL", false, 0);
					structure.addStructure("proc", "ENTIER", "LONGINT", 0, 0, 'P', "LONGREAL", false, 0);
					
					//EVEN
					structure.addStructure("proc", "EVEN", "BOOLEAN", 0, 0, 'P', "SHORTINT", false, 0);
					structure.addStructure("proc", "EVEN", "BOOLEAN", 0, 0, 'P', "INTEGER", false, 0);
					structure.addStructure("proc", "EVEN", "BOOLEAN", 0, 0, 'P', "LONGINT", false, 0);
					
					//LEN
					structure.addStructure("proc", "LEN", "LONGINT", 0, 0, 'P', "STRING", false, 0);
					structure.addStructure("proc", "LEN", "LONGINT", 0, 0, 'P', "BOOLEAN", true, 0);
					structure.addStructure("proc", "LEN", "LONGINT", 0, 0, 'P', "CHAR", true, 0);
					structure.addStructure("proc", "LEN", "LONGINT", 0, 0, 'P', "SHORTINT", true, 0);	
					structure.addStructure("proc", "LEN", "LONGINT", 0, 0, 'P', "INTEGER", true, 0);
					structure.addStructure("proc", "LEN", "LONGINT", 0, 0, 'P', "LONGINT", true, 0);
					structure.addStructure("proc", "LEN", "LONGINT", 0, 0, 'P', "REAL", true, 0);
					structure.addStructure("proc", "LEN", "LONGINT", 0, 0, 'P', "LONGREAL", true, 0);
					structure.addStructure("proc", "LEN", "LONGINT", 0, 0, 'P', "STRING", true, 0);
					
					//LONG
					structure.addStructure("proc", "LONG", "INTEGER", 0, 0, 'P', "SHORTINT", false, 0);
					structure.addStructure("proc", "LONG", "LONGINT", 0, 0, 'P', "INTEGER", false, 0);
					structure.addStructure("proc", "LONG", "LONGREAL", 0, 0, 'P', "REAL", false, 0);
					
					//LSH
					structure.addStructure("proc", "LSH", "LONGINT", 0, 0, 'P', "SHORTINT SHORTINT", false, 0);
					structure.addStructure("proc", "LSH", "LONGINT", 0, 0, 'P', "INTEGER INTEGER", false, 0);
					structure.addStructure("proc", "LSH", "LONGINT", 0, 0, 'P', "LONGINT LONGINT", false, 0);
					
					//ODD
					structure.addStructure("proc", "ODD", "BOOLEAN", 0, 0, 'P', "SHORTINT", false, 0);
					structure.addStructure("proc", "ODD", "BOOLEAN", 0, 0, 'P', "INTEGER", false, 0);
					structure.addStructure("proc", "ODD", "BOOLEAN", 0, 0, 'P', "LONGINT", false, 0);
					
					//ORD
					structure.addStructure("proc", "ORD", "INTEGER", 0, 0, 'P', "CHAR", false, 0);
					
					//SHORT
					structure.addStructure("proc", "SHORT", "INTEGER", 0, 0, 'P', "LONGINT", false, 0);
					structure.addStructure("proc", "SHORT", "SHORTINT", 0, 0, 'P', "INTEGER", false, 0);
					structure.addStructure("proc", "SHORT", "REAL", 0, 0, 'P', "LONGREAL", false, 0);
					
					//HALT
					structure.addStructure("proc", "HALT", null, 0, 0, 'P', "INTEGER", false, 0);
					
					//DEC
					structure.addStructure("proc", "DEC", null, 0, 0, 'P', "SHORTINT", false, 0);
					structure.addStructure("proc", "DEC", null, 0, 0, 'P', "INTEGER", false, 0);
					structure.addStructure("proc", "DEC", null, 0, 0, 'P', "LONGINT", false, 0);
					structure.addStructure("proc", "DEC", null, 0, 0, 'P', "SHORTINT SHORTINT", false, 0);
					structure.addStructure("proc", "DEC", null, 0, 0, 'P', "INTEGER INTEGER", false, 0);
					structure.addStructure("proc", "DEC", null, 0, 0, 'P', "LONGINT LONGINT", false, 0);
					
					//INC
					structure.addStructure("proc", "INC", null, 0, 0, 'P', "SHORTINT", false, 0);
					structure.addStructure("proc", "INC", null, 0, 0, 'P', "INTEGER", false, 0);
					structure.addStructure("proc", "INC", null, 0, 0, 'P', "LONGINT", false, 0);
					structure.addStructure("proc", "INC", null, 0, 0, 'P', "SHORTINT SHORTINT", false, 0);
					structure.addStructure("proc", "INC", null, 0, 0, 'P', "INTEGER INTEGER", false, 0);
					structure.addStructure("proc", "INC", null, 0, 0, 'P', "LONGINT LONGINT", false, 0);	
				
			}
			AST __t2298 = _t;
			AST tmp1_AST_in = (AST)_t;
			match(_t,LITERAL_MODULE);
			_t = _t.getFirstChild();
			ID1 = (AST)_t;
			match(_t,IDENT);
			_t = _t.getNextSibling();
			{
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case LITERAL_IMPORT:
			{
				importList(_t);
				_t = _retTree;
				break;
			}
			case IDENT:
			case LITERAL_BEGIN:
			case LITERAL_CONST:
			case LITERAL_TYPE:
			case LITERAL_VAR:
			case LITERAL_PROCEDURE:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
			}
			declarationSequence(_t);
			_t = _retTree;
			{
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case LITERAL_BEGIN:
			{
				mainBegin(_t);
				_t = _retTree;
				break;
			}
			case IDENT:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
			}
			ID2 = (AST)_t;
			match(_t,IDENT);
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
				
				//	if (!(I1.getText().equals(I2.getText())))
							//System.out.println("gyuij");
				//		new SemanticException("oiut");
				//	SemanticException a;
				//    a =  new SemanticException("kjhgfl");
				System.out.println("LOOL");
					
				
			}
			_t = __t2298;
			_t = _t.getNextSibling();
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void importList(AST _t) throws RecognitionException {
		
		AST importList_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2506 = _t;
			AST tmp2_AST_in = (AST)_t;
			match(_t,LITERAL_IMPORT);
			_t = _t.getFirstChild();
			obimport(_t);
			_t = _retTree;
			{
			_loop2508:
			do {
				if (_t==null) _t=ASTNULL;
				if ((_t.getType()==ASSIGN||_t.getType()==IDENT)) {
					obimport(_t);
					_t = _retTree;
				}
				else {
					break _loop2508;
				}
				
			} while (true);
			}
			_t = __t2506;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void declarationSequence(AST _t) throws RecognitionException {
		
		AST declarationSequence_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			{
			_loop2488:
			do {
				if (_t==null) _t=ASTNULL;
				switch ( _t.getType()) {
				case LITERAL_CONST:
				{
					constantDeclaration(_t);
					_t = _retTree;
					break;
				}
				case LITERAL_TYPE:
				{
					typeDeclaration(_t);
					_t = _retTree;
					break;
				}
				case LITERAL_VAR:
				{
					variableDeclaration(_t);
					_t = _retTree;
					break;
				}
				default:
				{
					break _loop2488;
				}
				}
			} while (true);
			}
			{
			_loop2494:
			do {
				boolean synPredMatched2491 = false;
				if (_t==null) _t=ASTNULL;
				if (((_t.getType()==LITERAL_PROCEDURE))) {
					AST __t2491 = _t;
					synPredMatched2491 = true;
					inputState.guessing++;
					try {
						{
						procedureDeclaration(_t);
						_t = _retTree;
						}
					}
					catch (RecognitionException pe) {
						synPredMatched2491 = false;
					}
					_t = __t2491;
inputState.guessing--;
				}
				if ( synPredMatched2491 ) {
					procedureDeclaration(_t);
					_t = _retTree;
				}
				else {
					boolean synPredMatched2493 = false;
					if (_t==null) _t=ASTNULL;
					if (((_t.getType()==LITERAL_PROCEDURE))) {
						AST __t2493 = _t;
						synPredMatched2493 = true;
						inputState.guessing++;
						try {
							{
							forwardDeclaration(_t);
							_t = _retTree;
							}
						}
						catch (RecognitionException pe) {
							synPredMatched2493 = false;
						}
						_t = __t2493;
inputState.guessing--;
					}
					if ( synPredMatched2493 ) {
						forwardDeclaration(_t);
						_t = _retTree;
					}
					else {
						break _loop2494;
					}
					}
				} while (true);
				}
			}
			catch (RecognitionException ex) {
				if (inputState.guessing==0) {
					reportError(ex);
					if (_t!=null) {_t = _t.getNextSibling();}
				} else {
				  throw ex;
				}
			}
			_retTree = _t;
		}
		
	public final void mainBegin(AST _t) throws RecognitionException {
		
		AST mainBegin_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			boolean synPredMatched2304 = false;
			if (_t==null) _t=ASTNULL;
			if (((_t.getType()==LITERAL_BEGIN))) {
				AST __t2304 = _t;
				synPredMatched2304 = true;
				inputState.guessing++;
				try {
					{
					AST __t2303 = _t;
					AST tmp3_AST_in = (AST)_t;
					match(_t,LITERAL_BEGIN);
					_t = _t.getFirstChild();
					statementSequence(_t);
					_t = _retTree;
					_t = __t2303;
					_t = _t.getNextSibling();
					}
				}
				catch (RecognitionException pe) {
					synPredMatched2304 = false;
				}
				_t = __t2304;
inputState.guessing--;
			}
			if ( synPredMatched2304 ) {
				AST __t2305 = _t;
				AST tmp4_AST_in = (AST)_t;
				match(_t,LITERAL_BEGIN);
				_t = _t.getFirstChild();
				statementSequence(_t);
				_t = _retTree;
				_t = __t2305;
				_t = _t.getNextSibling();
			}
			else if ((_t.getType()==LITERAL_BEGIN)) {
				AST tmp5_AST_in = (AST)_t;
				match(_t,LITERAL_BEGIN);
				_t = _t.getNextSibling();
				if ( inputState.guessing==0 ) {
				}
			}
			else {
				throw new NoViableAltException(_t);
			}
			
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void statementSequence(AST _t) throws RecognitionException {
		
		AST statementSequence_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			boolean synPredMatched2441 = false;
			if (_t==null) _t=ASTNULL;
			if (((_tokenSet_0.member(_t.getType())))) {
				AST __t2441 = _t;
				synPredMatched2441 = true;
				inputState.guessing++;
				try {
					{
					statement(_t);
					_t = _retTree;
					statement(_t);
					_t = _retTree;
					}
				}
				catch (RecognitionException pe) {
					synPredMatched2441 = false;
				}
				_t = __t2441;
inputState.guessing--;
			}
			if ( synPredMatched2441 ) {
				{
				statement(_t);
				_t = _retTree;
				statementSequence(_t);
				_t = _retTree;
				}
			}
			else if ((_tokenSet_0.member(_t.getType()))) {
				statement(_t);
				_t = _retTree;
			}
			else {
				throw new NoViableAltException(_t);
			}
			
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void identdef(AST _t) throws RecognitionException {
		
		AST identdef_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST tmp6_AST_in = (AST)_t;
			match(_t,IDENT);
			_t = _t.getNextSibling();
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void qualident(AST _t) throws RecognitionException {
		
		AST qualident_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case DOT:
			{
				AST __t2310 = _t;
				AST tmp7_AST_in = (AST)_t;
				match(_t,DOT);
				_t = _t.getFirstChild();
				AST tmp8_AST_in = (AST)_t;
				match(_t,IDENT);
				_t = _t.getNextSibling();
				AST tmp9_AST_in = (AST)_t;
				match(_t,IDENT);
				_t = _t.getNextSibling();
				_t = __t2310;
				_t = _t.getNextSibling();
				break;
			}
			case IDENT:
			{
				AST tmp10_AST_in = (AST)_t;
				match(_t,IDENT);
				_t = _t.getNextSibling();
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void constantDeclaration(AST _t) throws RecognitionException {
		
		AST constantDeclaration_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2312 = _t;
			AST tmp11_AST_in = (AST)_t;
			match(_t,LITERAL_CONST);
			_t = _t.getFirstChild();
			{
			int _cnt2314=0;
			_loop2314:
			do {
				if (_t==null) _t=ASTNULL;
				if ((_t.getType()==IDENT)) {
					identdef(_t);
					_t = _retTree;
					constExpression(_t);
					_t = _retTree;
				}
				else {
					if ( _cnt2314>=1 ) { break _loop2314; } else {throw new NoViableAltException(_t);}
				}
				
				_cnt2314++;
			} while (true);
			}
			_t = __t2312;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void constExpression(AST _t) throws RecognitionException {
		
		AST constExpression_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			expression(_t);
			_t = _retTree;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void expression(AST _t) throws RecognitionException {
		
		AST expression_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case EQUAL:
			{
				AST __t2348 = _t;
				AST tmp12_AST_in = (AST)_t;
				match(_t,EQUAL);
				_t = _t.getFirstChild();
				expression(_t);
				_t = _retTree;
				expression(_t);
				_t = _retTree;
				_t = __t2348;
				_t = _t.getNextSibling();
				break;
			}
			case NEQUAL:
			{
				AST __t2352 = _t;
				AST tmp13_AST_in = (AST)_t;
				match(_t,NEQUAL);
				_t = _t.getFirstChild();
				expression(_t);
				_t = _retTree;
				expression(_t);
				_t = _retTree;
				_t = __t2352;
				_t = _t.getNextSibling();
				break;
			}
			case LESS:
			{
				AST __t2356 = _t;
				AST tmp14_AST_in = (AST)_t;
				match(_t,LESS);
				_t = _t.getFirstChild();
				expression(_t);
				_t = _retTree;
				expression(_t);
				_t = _retTree;
				_t = __t2356;
				_t = _t.getNextSibling();
				break;
			}
			case LEQ:
			{
				AST __t2360 = _t;
				AST tmp15_AST_in = (AST)_t;
				match(_t,LEQ);
				_t = _t.getFirstChild();
				expression(_t);
				_t = _retTree;
				expression(_t);
				_t = _retTree;
				_t = __t2360;
				_t = _t.getNextSibling();
				break;
			}
			case GREATER:
			{
				AST __t2364 = _t;
				AST tmp16_AST_in = (AST)_t;
				match(_t,GREATER);
				_t = _t.getFirstChild();
				expression(_t);
				_t = _retTree;
				expression(_t);
				_t = _retTree;
				_t = __t2364;
				_t = _t.getNextSibling();
				break;
			}
			case GEQ:
			{
				AST __t2368 = _t;
				AST tmp17_AST_in = (AST)_t;
				match(_t,GEQ);
				_t = _t.getFirstChild();
				expression(_t);
				_t = _retTree;
				expression(_t);
				_t = _retTree;
				_t = __t2368;
				_t = _t.getNextSibling();
				break;
			}
			case LITERAL_IS:
			{
				AST __t2372 = _t;
				AST tmp18_AST_in = (AST)_t;
				match(_t,LITERAL_IS);
				_t = _t.getFirstChild();
				expression(_t);
				_t = _retTree;
				expression(_t);
				_t = _retTree;
				_t = __t2372;
				_t = _t.getNextSibling();
				break;
			}
			case STAR:
			case DOT:
			case LPAREN:
			case LBR:
			case PLUS:
			case MINUS:
			case SLASH:
			case AMP:
			case TILDE:
			case IDENT:
			case NUMBER:
			case STRING:
			case CHAR:
			case LITERAL_OR:
			case LITERAL_DIV:
			case LITERAL_MOD:
			case LITERAL_NIL:
			{
				simpleExpression(_t);
				_t = _retTree;
				if ( inputState.guessing==0 ) {
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void typeDeclaration(AST _t) throws RecognitionException {
		
		AST typeDeclaration_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2317 = _t;
			AST tmp19_AST_in = (AST)_t;
			match(_t,LITERAL_TYPE);
			_t = _t.getFirstChild();
			{
			int _cnt2319=0;
			_loop2319:
			do {
				if (_t==null) _t=ASTNULL;
				if ((_t.getType()==IDENT)) {
					identdef(_t);
					_t = _retTree;
					type(_t);
					_t = _retTree;
				}
				else {
					if ( _cnt2319>=1 ) { break _loop2319; } else {throw new NoViableAltException(_t);}
				}
				
				_cnt2319++;
			} while (true);
			}
			_t = __t2317;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void type(AST _t) throws RecognitionException {
		
		AST type_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case IDENT:
			{
				identdef(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_ARRAY:
			{
				arrayType(_t);
				_t = _retTree;
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void arrayType(AST _t) throws RecognitionException {
		
		AST arrayType_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2322 = _t;
			AST tmp20_AST_in = (AST)_t;
			match(_t,LITERAL_ARRAY);
			_t = _t.getFirstChild();
			length(_t);
			_t = _retTree;
			type(_t);
			_t = _retTree;
			_t = __t2322;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void length(AST _t) throws RecognitionException {
		
		AST length_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			constExpression(_t);
			_t = _retTree;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void identList(AST _t) throws RecognitionException {
		
		AST identList_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case COMMA:
			{
				AST __t2325 = _t;
				AST tmp21_AST_in = (AST)_t;
				match(_t,COMMA);
				_t = _t.getFirstChild();
				{
				_loop2327:
				do {
					if (_t==null) _t=ASTNULL;
					if ((_t.getType()==IDENT)) {
						identdef(_t);
						_t = _retTree;
					}
					else {
						break _loop2327;
					}
					
				} while (true);
				}
				_t = __t2325;
				_t = _t.getNextSibling();
				break;
			}
			case IDENT:
			{
				identdef(_t);
				_t = _retTree;
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void variableDeclaration(AST _t) throws RecognitionException {
		
		AST variableDeclaration_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2329 = _t;
			AST tmp22_AST_in = (AST)_t;
			match(_t,LITERAL_VAR);
			_t = _t.getFirstChild();
			{
			int _cnt2331=0;
			_loop2331:
			do {
				if (_t==null) _t=ASTNULL;
				if ((_t.getType()==COLON)) {
					varDec(_t);
					_t = _retTree;
				}
				else {
					if ( _cnt2331>=1 ) { break _loop2331; } else {throw new NoViableAltException(_t);}
				}
				
				_cnt2331++;
			} while (true);
			}
			_t = __t2329;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void varDec(AST _t) throws RecognitionException {
		
		AST varDec_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2333 = _t;
			AST tmp23_AST_in = (AST)_t;
			match(_t,COLON);
			_t = _t.getFirstChild();
			identList(_t);
			_t = _retTree;
			type(_t);
			_t = _retTree;
			_t = __t2333;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void designator(AST _t) throws RecognitionException {
		
		AST designator_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case LBR:
			{
				AST __t2338 = _t;
				AST tmp24_AST_in = (AST)_t;
				match(_t,LBR);
				_t = _t.getFirstChild();
				designator(_t);
				_t = _retTree;
				expList(_t);
				_t = _retTree;
				AST tmp25_AST_in = (AST)_t;
				match(_t,RBR);
				_t = _t.getNextSibling();
				_t = __t2338;
				_t = _t.getNextSibling();
				break;
			}
			case LPAREN:
			{
				AST __t2342 = _t;
				AST tmp26_AST_in = (AST)_t;
				match(_t,LPAREN);
				_t = _t.getFirstChild();
				designator(_t);
				_t = _retTree;
				qualident(_t);
				_t = _retTree;
				AST tmp27_AST_in = (AST)_t;
				match(_t,LPAREN);
				_t = _t.getNextSibling();
				_t = __t2342;
				_t = _t.getNextSibling();
				break;
			}
			case DOT:
			case IDENT:
			{
				qualident(_t);
				_t = _retTree;
				if ( inputState.guessing==0 ) {
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void expList(AST _t) throws RecognitionException {
		
		AST expList_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			expression(_t);
			_t = _retTree;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void simpleExpression(AST _t) throws RecognitionException {
		
		AST simpleExpression_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			boolean synPredMatched2376 = false;
			if (_t==null) _t=ASTNULL;
			if (((_tokenSet_1.member(_t.getType())))) {
				AST __t2376 = _t;
				synPredMatched2376 = true;
				inputState.guessing++;
				try {
					{
					AST __t2375 = _t;
					AST tmp28_AST_in = (AST)_t;
					match(_t,PLUS);
					_t = _t.getFirstChild();
					signDB(_t);
					_t = _retTree;
					signDB(_t);
					_t = _retTree;
					_t = __t2375;
					_t = _t.getNextSibling();
					}
				}
				catch (RecognitionException pe) {
					synPredMatched2376 = false;
				}
				_t = __t2376;
inputState.guessing--;
			}
			if ( synPredMatched2376 ) {
				signDB(_t);
				_t = _retTree;
			}
			else {
				boolean synPredMatched2379 = false;
				if (_t==null) _t=ASTNULL;
				if (((_tokenSet_1.member(_t.getType())))) {
					AST __t2379 = _t;
					synPredMatched2379 = true;
					inputState.guessing++;
					try {
						{
						AST __t2378 = _t;
						AST tmp29_AST_in = (AST)_t;
						match(_t,MINUS);
						_t = _t.getFirstChild();
						signDB(_t);
						_t = _retTree;
						signDB(_t);
						_t = _retTree;
						_t = __t2378;
						_t = _t.getNextSibling();
						}
					}
					catch (RecognitionException pe) {
						synPredMatched2379 = false;
					}
					_t = __t2379;
inputState.guessing--;
				}
				if ( synPredMatched2379 ) {
					signDB(_t);
					_t = _retTree;
				}
				else {
					boolean synPredMatched2382 = false;
					if (_t==null) _t=ASTNULL;
					if (((_t.getType()==PLUS))) {
						AST __t2382 = _t;
						synPredMatched2382 = true;
						inputState.guessing++;
						try {
							{
							AST __t2381 = _t;
							AST tmp30_AST_in = (AST)_t;
							match(_t,PLUS);
							_t = _t.getFirstChild();
							signDB(_t);
							_t = _retTree;
							_t = __t2381;
							_t = _t.getNextSibling();
							}
						}
						catch (RecognitionException pe) {
							synPredMatched2382 = false;
						}
						_t = __t2382;
inputState.guessing--;
					}
					if ( synPredMatched2382 ) {
						AST __t2383 = _t;
						AST tmp31_AST_in = (AST)_t;
						match(_t,PLUS);
						_t = _t.getFirstChild();
						signDB(_t);
						_t = _retTree;
						_t = __t2383;
						_t = _t.getNextSibling();
					}
					else {
						boolean synPredMatched2386 = false;
						if (_t==null) _t=ASTNULL;
						if (((_t.getType()==MINUS))) {
							AST __t2386 = _t;
							synPredMatched2386 = true;
							inputState.guessing++;
							try {
								{
								AST __t2385 = _t;
								AST tmp32_AST_in = (AST)_t;
								match(_t,MINUS);
								_t = _t.getFirstChild();
								signDB(_t);
								_t = _retTree;
								_t = __t2385;
								_t = _t.getNextSibling();
								}
							}
							catch (RecognitionException pe) {
								synPredMatched2386 = false;
							}
							_t = __t2386;
inputState.guessing--;
						}
						if ( synPredMatched2386 ) {
							AST __t2387 = _t;
							AST tmp33_AST_in = (AST)_t;
							match(_t,MINUS);
							_t = _t.getFirstChild();
							signDB(_t);
							_t = _retTree;
							_t = __t2387;
							_t = _t.getNextSibling();
						}
						else if ((_tokenSet_1.member(_t.getType()))) {
							signDB(_t);
							_t = _retTree;
							if ( inputState.guessing==0 ) {
							}
						}
						else {
							throw new NoViableAltException(_t);
						}
						}}}
					}
					catch (RecognitionException ex) {
						if (inputState.guessing==0) {
							reportError(ex);
							if (_t!=null) {_t = _t.getNextSibling();}
						} else {
						  throw ex;
						}
					}
					_retTree = _t;
				}
				
	public final void signDB(AST _t) throws RecognitionException {
		
		AST signDB_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case PLUS:
			{
				AST __t2389 = _t;
				AST tmp34_AST_in = (AST)_t;
				match(_t,PLUS);
				_t = _t.getFirstChild();
				signDB(_t);
				_t = _retTree;
				term(_t);
				_t = _retTree;
				_t = __t2389;
				_t = _t.getNextSibling();
				break;
			}
			case MINUS:
			{
				AST __t2390 = _t;
				AST tmp35_AST_in = (AST)_t;
				match(_t,MINUS);
				_t = _t.getFirstChild();
				signDB(_t);
				_t = _retTree;
				term(_t);
				_t = _retTree;
				_t = __t2390;
				_t = _t.getNextSibling();
				break;
			}
			case LITERAL_OR:
			{
				AST __t2391 = _t;
				AST tmp36_AST_in = (AST)_t;
				match(_t,LITERAL_OR);
				_t = _t.getFirstChild();
				signDB(_t);
				_t = _retTree;
				term(_t);
				_t = _retTree;
				_t = __t2391;
				_t = _t.getNextSibling();
				break;
			}
			case STAR:
			case DOT:
			case LPAREN:
			case LBR:
			case SLASH:
			case AMP:
			case TILDE:
			case IDENT:
			case NUMBER:
			case STRING:
			case CHAR:
			case LITERAL_DIV:
			case LITERAL_MOD:
			case LITERAL_NIL:
			{
				term(_t);
				_t = _retTree;
				if ( inputState.guessing==0 ) {
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void term(AST _t) throws RecognitionException {
		
		AST term_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case STAR:
			{
				AST __t2393 = _t;
				AST tmp37_AST_in = (AST)_t;
				match(_t,STAR);
				_t = _t.getFirstChild();
				term(_t);
				_t = _retTree;
				factor(_t);
				_t = _retTree;
				_t = __t2393;
				_t = _t.getNextSibling();
				break;
			}
			case SLASH:
			{
				AST __t2394 = _t;
				AST tmp38_AST_in = (AST)_t;
				match(_t,SLASH);
				_t = _t.getFirstChild();
				term(_t);
				_t = _retTree;
				factor(_t);
				_t = _retTree;
				_t = __t2394;
				_t = _t.getNextSibling();
				break;
			}
			case LITERAL_DIV:
			{
				AST __t2395 = _t;
				AST tmp39_AST_in = (AST)_t;
				match(_t,LITERAL_DIV);
				_t = _t.getFirstChild();
				term(_t);
				_t = _retTree;
				factor(_t);
				_t = _retTree;
				_t = __t2395;
				_t = _t.getNextSibling();
				break;
			}
			case LITERAL_MOD:
			{
				AST __t2396 = _t;
				AST tmp40_AST_in = (AST)_t;
				match(_t,LITERAL_MOD);
				_t = _t.getFirstChild();
				term(_t);
				_t = _retTree;
				factor(_t);
				_t = _retTree;
				_t = __t2396;
				_t = _t.getNextSibling();
				break;
			}
			case AMP:
			{
				AST __t2397 = _t;
				AST tmp41_AST_in = (AST)_t;
				match(_t,AMP);
				_t = _t.getFirstChild();
				term(_t);
				_t = _retTree;
				factor(_t);
				_t = _retTree;
				_t = __t2397;
				_t = _t.getNextSibling();
				break;
			}
			case DOT:
			case LPAREN:
			case LBR:
			case TILDE:
			case IDENT:
			case NUMBER:
			case STRING:
			case CHAR:
			case LITERAL_NIL:
			{
				factor(_t);
				_t = _retTree;
				if ( inputState.guessing==0 ) {
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void factor(AST _t) throws RecognitionException {
		
		AST factor_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case NUMBER:
			{
				AST tmp42_AST_in = (AST)_t;
				match(_t,NUMBER);
				_t = _t.getNextSibling();
				break;
			}
			case CHAR:
			{
				AST tmp43_AST_in = (AST)_t;
				match(_t,CHAR);
				_t = _t.getNextSibling();
				break;
			}
			case STRING:
			{
				AST tmp44_AST_in = (AST)_t;
				match(_t,STRING);
				_t = _t.getNextSibling();
				break;
			}
			case LITERAL_NIL:
			{
				AST tmp45_AST_in = (AST)_t;
				match(_t,LITERAL_NIL);
				_t = _t.getNextSibling();
				break;
			}
			case TILDE:
			{
				AST __t2405 = _t;
				AST tmp46_AST_in = (AST)_t;
				match(_t,TILDE);
				_t = _t.getFirstChild();
				factor(_t);
				_t = _retTree;
				_t = __t2405;
				_t = _t.getNextSibling();
				break;
			}
			default:
				boolean synPredMatched2401 = false;
				if (_t==null) _t=ASTNULL;
				if (((_t.getType()==DOT||_t.getType()==IDENT))) {
					AST __t2401 = _t;
					synPredMatched2401 = true;
					inputState.guessing++;
					try {
						{
						AST __t2400 = _t;
						AST tmp47_AST_in = (AST)_t;
						match(_t,IDENT);
						_t = _t.getFirstChild();
						actualParameters(_t);
						_t = _retTree;
						_t = __t2400;
						_t = _t.getNextSibling();
						}
					}
					catch (RecognitionException pe) {
						synPredMatched2401 = false;
					}
					_t = __t2401;
inputState.guessing--;
				}
				if ( synPredMatched2401 ) {
					procedureCall(_t);
					_t = _retTree;
				}
				else {
					boolean synPredMatched2404 = false;
					if (_t==null) _t=ASTNULL;
					if (((_t.getType()==DOT||_t.getType()==IDENT))) {
						AST __t2404 = _t;
						synPredMatched2404 = true;
						inputState.guessing++;
						try {
							{
							AST __t2403 = _t;
							AST tmp48_AST_in = (AST)_t;
							match(_t,IDENT);
							_t = _t.getFirstChild();
							AST tmp49_AST_in = (AST)_t;
							match(_t,DOT);
							_t = _t.getNextSibling();
							_t = __t2403;
							_t = _t.getNextSibling();
							}
						}
						catch (RecognitionException pe) {
							synPredMatched2404 = false;
						}
						_t = __t2404;
inputState.guessing--;
					}
					if ( synPredMatched2404 ) {
						procedureCall(_t);
						_t = _retTree;
					}
					else {
						boolean synPredMatched2408 = false;
						if (_t==null) _t=ASTNULL;
						if (((_t.getType()==LPAREN))) {
							AST __t2408 = _t;
							synPredMatched2408 = true;
							inputState.guessing++;
							try {
								{
								AST __t2407 = _t;
								AST tmp50_AST_in = (AST)_t;
								match(_t,LPAREN);
								_t = _t.getFirstChild();
								expression(_t);
								_t = _retTree;
								AST tmp51_AST_in = (AST)_t;
								match(_t,RPAREN);
								_t = _t.getNextSibling();
								_t = __t2407;
								_t = _t.getNextSibling();
								}
							}
							catch (RecognitionException pe) {
								synPredMatched2408 = false;
							}
							_t = __t2408;
inputState.guessing--;
						}
						if ( synPredMatched2408 ) {
							AST __t2409 = _t;
							AST tmp52_AST_in = (AST)_t;
							match(_t,LPAREN);
							_t = _t.getFirstChild();
							expression(_t);
							_t = _retTree;
							AST tmp53_AST_in = (AST)_t;
							match(_t,RPAREN);
							_t = _t.getNextSibling();
							_t = __t2409;
							_t = _t.getNextSibling();
						}
						else if ((_tokenSet_2.member(_t.getType()))) {
							designator(_t);
							_t = _retTree;
						}
					else {
						throw new NoViableAltException(_t);
					}
					}}}
				}
				catch (RecognitionException ex) {
					if (inputState.guessing==0) {
						reportError(ex);
						if (_t!=null) {_t = _t.getNextSibling();}
					} else {
					  throw ex;
					}
				}
				_retTree = _t;
			}
			
	public final void actualParameters(AST _t) throws RecognitionException {
		
		AST actualParameters_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			expList(_t);
			_t = _retTree;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void procedureCall(AST _t) throws RecognitionException {
		
		AST procedureCall_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			boolean synPredMatched2434 = false;
			if (_t==null) _t=ASTNULL;
			if (((_t.getType()==IDENT))) {
				AST __t2434 = _t;
				synPredMatched2434 = true;
				inputState.guessing++;
				try {
					{
					AST tmp54_AST_in = (AST)_t;
					match(_t,IDENT);
					_t = _t.getNextSibling();
					actualParameters(_t);
					_t = _retTree;
					}
				}
				catch (RecognitionException pe) {
					synPredMatched2434 = false;
				}
				_t = __t2434;
inputState.guessing--;
			}
			if ( synPredMatched2434 ) {
				AST tmp55_AST_in = (AST)_t;
				match(_t,IDENT);
				_t = _t.getNextSibling();
				actualParameters(_t);
				_t = _retTree;
			}
			else if ((_t.getType()==IDENT)) {
				AST tmp56_AST_in = (AST)_t;
				match(_t,IDENT);
				_t = _t.getNextSibling();
			}
			else {
				boolean synPredMatched2436 = false;
				if (_t==null) _t=ASTNULL;
				if (((_t.getType()==DOT))) {
					AST __t2436 = _t;
					synPredMatched2436 = true;
					inputState.guessing++;
					try {
						{
						AST tmp57_AST_in = (AST)_t;
						match(_t,DOT);
						_t = _t.getNextSibling();
						AST tmp58_AST_in = (AST)_t;
						match(_t,IDENT);
						_t = _t.getNextSibling();
						AST tmp59_AST_in = (AST)_t;
						match(_t,IDENT);
						_t = _t.getNextSibling();
						actualParameters(_t);
						_t = _retTree;
						}
					}
					catch (RecognitionException pe) {
						synPredMatched2436 = false;
					}
					_t = __t2436;
inputState.guessing--;
				}
				if ( synPredMatched2436 ) {
					AST __t2437 = _t;
					AST tmp60_AST_in = (AST)_t;
					match(_t,DOT);
					_t = _t.getFirstChild();
					AST tmp61_AST_in = (AST)_t;
					match(_t,IDENT);
					_t = _t.getNextSibling();
					AST tmp62_AST_in = (AST)_t;
					match(_t,IDENT);
					_t = _t.getNextSibling();
					actualParameters(_t);
					_t = _retTree;
					_t = __t2437;
					_t = _t.getNextSibling();
				}
				else if ((_t.getType()==DOT)) {
					AST __t2438 = _t;
					AST tmp63_AST_in = (AST)_t;
					match(_t,DOT);
					_t = _t.getFirstChild();
					AST tmp64_AST_in = (AST)_t;
					match(_t,IDENT);
					_t = _t.getNextSibling();
					AST tmp65_AST_in = (AST)_t;
					match(_t,IDENT);
					_t = _t.getNextSibling();
					_t = __t2438;
					_t = _t.getNextSibling();
					if ( inputState.guessing==0 ) {
					}
				}
				else {
					throw new NoViableAltException(_t);
				}
				}
			}
			catch (RecognitionException ex) {
				if (inputState.guessing==0) {
					reportError(ex);
					if (_t!=null) {_t = _t.getNextSibling();}
				} else {
				  throw ex;
				}
			}
			_retTree = _t;
		}
		
	public final void statement(AST _t) throws RecognitionException {
		
		AST statement_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case ASSIGN:
			{
				assignment(_t);
				_t = _retTree;
				break;
			}
			case DOT:
			case IDENT:
			{
				procedureCall(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_IF:
			{
				ifStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_WHILE:
			{
				whileStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_REPEAT:
			{
				repeatStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_FOR:
			{
				forStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_LOOP:
			{
				loopStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_EXIT:
			{
				AST tmp66_AST_in = (AST)_t;
				match(_t,LITERAL_EXIT);
				_t = _t.getNextSibling();
				break;
			}
			default:
				boolean synPredMatched2416 = false;
				if (_t==null) _t=ASTNULL;
				if (((_t.getType()==LITERAL_RETURN))) {
					AST __t2416 = _t;
					synPredMatched2416 = true;
					inputState.guessing++;
					try {
						{
						AST __t2415 = _t;
						AST tmp67_AST_in = (AST)_t;
						match(_t,LITERAL_RETURN);
						_t = _t.getFirstChild();
						expression(_t);
						_t = _retTree;
						_t = __t2415;
						_t = _t.getNextSibling();
						}
					}
					catch (RecognitionException pe) {
						synPredMatched2416 = false;
					}
					_t = __t2416;
inputState.guessing--;
				}
				if ( synPredMatched2416 ) {
					AST __t2417 = _t;
					AST tmp68_AST_in = (AST)_t;
					match(_t,LITERAL_RETURN);
					_t = _t.getFirstChild();
					expression(_t);
					_t = _retTree;
					_t = __t2417;
					_t = _t.getNextSibling();
				}
				else if ((_t.getType()==LITERAL_RETURN)) {
					AST tmp69_AST_in = (AST)_t;
					match(_t,LITERAL_RETURN);
					_t = _t.getNextSibling();
					if ( inputState.guessing==0 ) {
					}
				}
			else {
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void assignment(AST _t) throws RecognitionException {
		
		AST assignment_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2431 = _t;
			AST tmp70_AST_in = (AST)_t;
			match(_t,ASSIGN);
			_t = _t.getFirstChild();
			designator(_t);
			_t = _retTree;
			expression(_t);
			_t = _retTree;
			_t = __t2431;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void ifStatement(AST _t) throws RecognitionException {
		
		AST ifStatement_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2444 = _t;
			AST tmp71_AST_in = (AST)_t;
			match(_t,LITERAL_IF);
			_t = _t.getFirstChild();
			thenExpression(_t);
			_t = _retTree;
			{
			_loop2446:
			do {
				if (_t==null) _t=ASTNULL;
				if ((_t.getType()==LITERAL_ELSIF)) {
					elseIfStatement(_t);
					_t = _retTree;
				}
				else {
					break _loop2446;
				}
				
			} while (true);
			}
			{
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case LITERAL_ELSE:
			{
				elseStatement(_t);
				_t = _retTree;
				break;
			}
			case 3:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
			}
			_t = __t2444;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void whileStatement(AST _t) throws RecognitionException {
		
		AST whileStatement_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2455 = _t;
			AST tmp72_AST_in = (AST)_t;
			match(_t,LITERAL_WHILE);
			_t = _t.getFirstChild();
			expression(_t);
			_t = _retTree;
			statementSequence(_t);
			_t = _retTree;
			_t = __t2455;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void repeatStatement(AST _t) throws RecognitionException {
		
		AST repeatStatement_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2457 = _t;
			AST tmp73_AST_in = (AST)_t;
			match(_t,LITERAL_REPEAT);
			_t = _t.getFirstChild();
			statementSequence(_t);
			_t = _retTree;
			expression(_t);
			_t = _retTree;
			_t = __t2457;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void forStatement(AST _t) throws RecognitionException {
		
		AST forStatement_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2461 = _t;
			AST tmp74_AST_in = (AST)_t;
			match(_t,LITERAL_FOR);
			_t = _t.getFirstChild();
			forInit(_t);
			_t = _retTree;
			expression(_t);
			_t = _retTree;
			{
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case LITERAL_BY:
			{
				byExp(_t);
				_t = _retTree;
				break;
			}
			case DOT:
			case ASSIGN:
			case IDENT:
			case LITERAL_EXIT:
			case LITERAL_RETURN:
			case LITERAL_IF:
			case LITERAL_WHILE:
			case LITERAL_REPEAT:
			case LITERAL_FOR:
			case LITERAL_LOOP:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
			}
			statementSequence(_t);
			_t = _retTree;
			_t = __t2461;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void loopStatement(AST _t) throws RecognitionException {
		
		AST loopStatement_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2459 = _t;
			AST tmp75_AST_in = (AST)_t;
			match(_t,LITERAL_LOOP);
			_t = _t.getFirstChild();
			statementSequence(_t);
			_t = _retTree;
			_t = __t2459;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void statementNO(AST _t) throws RecognitionException {
		
		AST statementNO_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			{
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case ASSIGN:
			{
				assignment(_t);
				_t = _retTree;
				break;
			}
			case DOT:
			case IDENT:
			{
				procedureCall(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_IF:
			{
				ifStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_WHILE:
			{
				whileStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_REPEAT:
			{
				repeatStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_FOR:
			{
				forStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_LOOP:
			{
				loopStatement(_t);
				_t = _retTree;
				break;
			}
			case LITERAL_EXIT:
			{
				AST tmp76_AST_in = (AST)_t;
				match(_t,LITERAL_EXIT);
				_t = _t.getNextSibling();
				break;
			}
			case 3:
			{
				break;
			}
			default:
				boolean synPredMatched2424 = false;
				if (_t==null) _t=ASTNULL;
				if (((_t.getType()==LITERAL_RETURN))) {
					AST __t2424 = _t;
					synPredMatched2424 = true;
					inputState.guessing++;
					try {
						{
						AST __t2423 = _t;
						AST tmp77_AST_in = (AST)_t;
						match(_t,LITERAL_RETURN);
						_t = _t.getFirstChild();
						expression(_t);
						_t = _retTree;
						_t = __t2423;
						_t = _t.getNextSibling();
						}
					}
					catch (RecognitionException pe) {
						synPredMatched2424 = false;
					}
					_t = __t2424;
inputState.guessing--;
				}
				if ( synPredMatched2424 ) {
					AST __t2425 = _t;
					AST tmp78_AST_in = (AST)_t;
					match(_t,LITERAL_RETURN);
					_t = _t.getFirstChild();
					expression(_t);
					_t = _retTree;
					_t = __t2425;
					_t = _t.getNextSibling();
				}
				else if ((_t.getType()==LITERAL_RETURN)) {
					AST tmp79_AST_in = (AST)_t;
					match(_t,LITERAL_RETURN);
					_t = _t.getNextSibling();
				}
			else {
				throw new NoViableAltException(_t);
			}
			}
			}
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void returnBlock(AST _t) throws RecognitionException {
		
		AST returnBlock_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			boolean synPredMatched2428 = false;
			if (_t==null) _t=ASTNULL;
			if (((_t.getType()==LITERAL_RETURN))) {
				AST __t2428 = _t;
				synPredMatched2428 = true;
				inputState.guessing++;
				try {
					{
					AST tmp80_AST_in = (AST)_t;
					match(_t,LITERAL_RETURN);
					_t = _t.getNextSibling();
					expression(_t);
					_t = _retTree;
					}
				}
				catch (RecognitionException pe) {
					synPredMatched2428 = false;
				}
				_t = __t2428;
inputState.guessing--;
			}
			if ( synPredMatched2428 ) {
				AST __t2429 = _t;
				AST tmp81_AST_in = (AST)_t;
				match(_t,LITERAL_RETURN);
				_t = _t.getFirstChild();
				expression(_t);
				_t = _retTree;
				_t = __t2429;
				_t = _t.getNextSibling();
			}
			else if ((_t.getType()==LITERAL_RETURN)) {
				AST tmp82_AST_in = (AST)_t;
				match(_t,LITERAL_RETURN);
				_t = _t.getNextSibling();
			}
			else {
				throw new NoViableAltException(_t);
			}
			
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void thenExpression(AST _t) throws RecognitionException {
		
		AST thenExpression_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2449 = _t;
			AST tmp83_AST_in = (AST)_t;
			match(_t,LITERAL_THEN);
			_t = _t.getFirstChild();
			expression(_t);
			_t = _retTree;
			statementSequence(_t);
			_t = _retTree;
			_t = __t2449;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void elseIfStatement(AST _t) throws RecognitionException {
		
		AST elseIfStatement_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2451 = _t;
			AST tmp84_AST_in = (AST)_t;
			match(_t,LITERAL_ELSIF);
			_t = _t.getFirstChild();
			thenExpression(_t);
			_t = _retTree;
			_t = __t2451;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void elseStatement(AST _t) throws RecognitionException {
		
		AST elseStatement_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2453 = _t;
			AST tmp85_AST_in = (AST)_t;
			match(_t,LITERAL_ELSE);
			_t = _t.getFirstChild();
			statementSequence(_t);
			_t = _retTree;
			_t = __t2453;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void forInit(AST _t) throws RecognitionException {
		
		AST forInit_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2465 = _t;
			AST tmp86_AST_in = (AST)_t;
			match(_t,ASSIGN);
			_t = _t.getFirstChild();
			AST tmp87_AST_in = (AST)_t;
			match(_t,IDENT);
			_t = _t.getNextSibling();
			expression(_t);
			_t = _retTree;
			_t = __t2465;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void byExp(AST _t) throws RecognitionException {
		
		AST byExp_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST tmp88_AST_in = (AST)_t;
			match(_t,LITERAL_BY);
			_t = _t.getNextSibling();
			expression(_t);
			_t = _retTree;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void procedureDeclaration(AST _t) throws RecognitionException {
		
		AST procedureDeclaration_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2467 = _t;
			AST tmp89_AST_in = (AST)_t;
			match(_t,LITERAL_PROCEDURE);
			_t = _t.getFirstChild();
			procedureHeading(_t);
			_t = _retTree;
			procedureBody(_t);
			_t = _retTree;
			AST tmp90_AST_in = (AST)_t;
			match(_t,IDENT);
			_t = _t.getNextSibling();
			_t = __t2467;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void procedureHeading(AST _t) throws RecognitionException {
		
		AST procedureHeading_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2469 = _t;
			AST tmp91_AST_in = (AST)_t;
			match(_t,IDENT);
			_t = _t.getFirstChild();
			{
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case LPAREN:
			case COLON:
			{
				{
				if (_t==null) _t=ASTNULL;
				switch ( _t.getType()) {
				case COLON:
				{
					AST tmp92_AST_in = (AST)_t;
					match(_t,COLON);
					_t = _t.getNextSibling();
					qualident(_t);
					_t = _retTree;
					break;
				}
				case LPAREN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(_t);
				}
				}
				}
				AST tmp93_AST_in = (AST)_t;
				match(_t,LPAREN);
				_t = _t.getNextSibling();
				{
				if (_t==null) _t=ASTNULL;
				switch ( _t.getType()) {
				case DOT:
				case IDENT:
				case LITERAL_ARRAY:
				case LITERAL_VAR:
				{
					fParams(_t);
					_t = _retTree;
					break;
				}
				case RPAREN:
				{
					break;
				}
				default:
				{
					throw new NoViableAltException(_t);
				}
				}
				}
				AST tmp94_AST_in = (AST)_t;
				match(_t,RPAREN);
				_t = _t.getNextSibling();
				break;
			}
			case 3:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
			}
			_t = __t2469;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void procedureBody(AST _t) throws RecognitionException {
		
		AST procedureBody_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			declarationSequence(_t);
			_t = _retTree;
			{
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case LITERAL_BEGIN:
			{
				body(_t);
				_t = _retTree;
				break;
			}
			case IDENT:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void fParams(AST _t) throws RecognitionException {
		
		AST fParams_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			fVarSection(_t);
			_t = _retTree;
			{
			_loop2497:
			do {
				if (_t==null) _t=ASTNULL;
				if ((_tokenSet_3.member(_t.getType()))) {
					fVarSection(_t);
					_t = _retTree;
				}
				else {
					break _loop2497;
				}
				
			} while (true);
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void body(AST _t) throws RecognitionException {
		
		AST body_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2476 = _t;
			AST tmp95_AST_in = (AST)_t;
			match(_t,LITERAL_BEGIN);
			_t = _t.getFirstChild();
			{
			statement(_t);
			_t = _retTree;
			}
			_t = __t2476;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void forwardDeclaration(AST _t) throws RecognitionException {
		
		AST forwardDeclaration_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			AST __t2479 = _t;
			AST tmp96_AST_in = (AST)_t;
			match(_t,LITERAL_PROCEDURE);
			_t = _t.getFirstChild();
			AST tmp97_AST_in = (AST)_t;
			match(_t,REF);
			_t = _t.getNextSibling();
			procedureHeading(_t);
			_t = _retTree;
			_t = __t2479;
			_t = _t.getNextSibling();
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void fVarSection(AST _t) throws RecognitionException {
		
		AST fVarSection_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case LITERAL_VAR:
			{
				AST __t2499 = _t;
				AST tmp98_AST_in = (AST)_t;
				match(_t,LITERAL_VAR);
				_t = _t.getFirstChild();
				fArraySection(_t);
				_t = _retTree;
				_t = __t2499;
				_t = _t.getNextSibling();
				break;
			}
			case DOT:
			case IDENT:
			case LITERAL_ARRAY:
			{
				fArraySection(_t);
				_t = _retTree;
				if ( inputState.guessing==0 ) {
				}
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void fArraySection(AST _t) throws RecognitionException {
		
		AST fArraySection_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			formalType(_t);
			_t = _retTree;
			identList(_t);
			_t = _retTree;
			if ( inputState.guessing==0 ) {
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void formalType(AST _t) throws RecognitionException {
		
		AST formalType_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		
		try {      // for error handling
			{
			_loop2504:
			do {
				if (_t==null) _t=ASTNULL;
				if ((_t.getType()==LITERAL_ARRAY)) {
					AST __t2503 = _t;
					AST tmp99_AST_in = (AST)_t;
					match(_t,LITERAL_ARRAY);
					_t = _t.getFirstChild();
					AST tmp100_AST_in = (AST)_t;
					match(_t,LITERAL_OF);
					_t = _t.getNextSibling();
					_t = __t2503;
					_t = _t.getNextSibling();
				}
				else {
					break _loop2504;
				}
				
			} while (true);
			}
			qualident(_t);
			_t = _retTree;
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	public final void obimport(AST _t) throws RecognitionException {
		
		AST obimport_AST_in = (_t == ASTNULL) ? null : (AST)_t;
		AST L1 = null;
		AST L2 = null;
		AST L3 = null;
		
		try {      // for error handling
			if (_t==null) _t=ASTNULL;
			switch ( _t.getType()) {
			case ASSIGN:
			{
				AST __t2510 = _t;
				AST tmp101_AST_in = (AST)_t;
				match(_t,ASSIGN);
				_t = _t.getFirstChild();
				L1 = (AST)_t;
				match(_t,IDENT);
				_t = _t.getNextSibling();
				L2 = (AST)_t;
				match(_t,IDENT);
				_t = _t.getNextSibling();
				_t = __t2510;
				_t = _t.getNextSibling();
				break;
			}
			case IDENT:
			{
				L3 = (AST)_t;
				match(_t,IDENT);
				_t = _t.getNextSibling();
				break;
			}
			default:
			{
				throw new NoViableAltException(_t);
			}
			}
		}
		catch (RecognitionException ex) {
			if (inputState.guessing==0) {
				reportError(ex);
				if (_t!=null) {_t = _t.getNextSibling();}
			} else {
			  throw ex;
			}
		}
		_retTree = _t;
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
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
		"SPACES",
		"IDENT",
		"NUMBER",
		"COMMENT",
		"STRING",
		"DIGIT",
		"HEXDIGIT",
		"ORDER",
		"INT",
		"REAL",
		"LETTER",
		"CHAR",
		"\"MODULE\"",
		"\"END\"",
		"\"BEGIN\"",
		"\"CONST\"",
		"\"TYPE\"",
		"\"ARRAY\"",
		"\"OF\"",
		"\"VAR\"",
		"\"IS\"",
		"\"OR\"",
		"\"DIV\"",
		"\"MOD\"",
		"\"NIL\"",
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
		"\"FOR\"",
		"\"TO\"",
		"\"BY\"",
		"\"LOOP\"",
		"\"PROCEDURE\"",
		"\"IMPORT\""
	};
	
	private static final long[] mk_tokenSet_0() {
		long[] data = { -6665327445823979488L, 18L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_0 = new BitSet(mk_tokenSet_0());
	private static final long[] mk_tokenSet_1() {
		long[] data = { 33779220110910000L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_1 = new BitSet(mk_tokenSet_1());
	private static final long[] mk_tokenSet_2() {
		long[] data = { 2147492384L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_2 = new BitSet(mk_tokenSet_2());
	private static final long[] mk_tokenSet_3() {
		long[] data = { 703689589260320L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_3 = new BitSet(mk_tokenSet_3());
	}
	
