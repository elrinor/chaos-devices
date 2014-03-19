package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Stack;
import java.util.Collections;

import antlr.Token;

public class Module {
	public static boolean uses = false;
	public static String usesName = "";
	public static int usesCol = 0;
	public static int usesLine = 0;
	
	public static ArrayList<Identifier> idents = new ArrayList<Identifier>();
	public static Stack<Integer> funcStack = new Stack<Integer>();
	private static Stack<HashMap<String, Integer>> identNamesStack = new Stack<HashMap<String, Integer>>();
	private static Stack<ArrayList<Integer>> localsStack = new Stack<ArrayList<Integer>>();
	static {
		// init
		identNamesStack.add(new HashMap<String, Integer>());
		localsStack.add(new ArrayList<Integer>());
		try {
			addIdent(new Identifier(Type.Type, Type.ShortInt), "SHORTINT");
			addIdent(new Identifier(Type.Type, Type.Integer),  "INTEGER");
			addIdent(new Identifier(Type.Type, Type.LongInt),  "LONGINT");
			addIdent(new Identifier(Type.Type, Type.Real),     "REAL");
			addIdent(new Identifier(Type.Type, Type.LongReal), "LONGREAL");
			addIdent(new Identifier(Type.Type, Type.Boolean),  "BOOLEAN");
			addIdent(new Identifier(Type.Type, Type.String),   "STRING");
			addIdent(new Identifier(Type.Type, Type.Char),     "CHAR");
			addIdent(new Identifier(Type.Boolean, true),       "TRUE");
			addIdent(new Identifier(Type.Boolean, false),      "FALSE");
		} catch (Error e) {/* it won't happen... maybe */}
	}

	public static int currentFuncNo() {
		return funcStack.lastElement();
	}
	
	public static int getIdentNo(Token nameTok) throws Error {
		for(int i = identNamesStack.size() - 1; i >= 0; i--)
			if(identNamesStack.get(i).containsKey(nameTok.getText())) {
				Identifier ident = idents.get(identNamesStack.get(i).get(nameTok.getText()));
				ident.usages++;
				if(uses && usesName.equals(nameTok.getText()) && (usesCol == 0 || usesCol == ident.defCol) && (usesLine == 0 || usesLine == ident.defLine))
					System.out.println(nameTok.getLine() + ":" + nameTok.getColumn() + ":" + usesName);
				return identNamesStack.get(i).get(nameTok.getText());
			}
		throw new Error(Error.COMPILEERROR, "Identifier \"" + nameTok.getText() + "\" not found");
	}
	public static void enterFunction(int funcNo) {
		funcStack.push(funcNo);
		identNamesStack.push(new HashMap<String, Integer>());
		localsStack.push(new ArrayList<Integer>());
	}
	public static void enterFunction(Identifier funcIdent) {
		enterFunction(funcIdent.getNo());
	}
	public static ArrayList<Integer> leaveFunction() {
		identNamesStack.pop();
		return localsStack.pop();
	}
	
	public static Statement main; // must contain call main();
	
	public static void checkForwardDecl(Identifier ident) throws Error {
		Identifier oldVar = idents.get(ident.getNo());
		if(oldVar.getType().typeId() == Type.FUNCTION && oldVar.getValue() == null && ident.getType().equals(oldVar.getType()))
			oldVar.setValue(ident.getValue());
		else if(oldVar.getValue() == null)
			throw new Error(Error.COMPILEERROR, "Function redefinition");
	}
	
	public static void addIdent(Identifier ident, String name, int pos) throws Error {
		if(identNamesStack.get(pos).containsKey(name)) {
			Identifier oldVar = idents.get(identNamesStack.get(pos).get(name));
			if(oldVar.getType().typeId() == Type.FUNCTION && oldVar.getValue() == null)
				ident.setNo(oldVar.getNo());
			else
				throw new Error(Error.COMPILEERROR, "Identifier \"" + name + "\" redefinition");
		} else {
			ident.setNo(idents.size());
			identNamesStack.get(pos).put(name, ident.getNo());
			localsStack.get(pos).add(ident.getNo());
			idents.add(ident);
		}
		ident.name = name;
	}
	
	public static void addIdent(Identifier ident, String name) throws Error {
		addIdent(ident, name, identNamesStack.size() - 1);
	}
	
	public static void addTempIdent(Identifier ident) throws Error {
		addIdent(ident, "^_^@" + idents.size());
	}
	
	public static void addIdent(Identifier ident, Token defToken) throws Error {
		ident.defLine = defToken.getLine();
		ident.defCol = defToken.getColumn();
		addIdent(ident, defToken.getText());
	}

	public static void Import(String name) throws Error {
		if(name.equals("In")) {
			addIdent(new Identifier(Type.Function(Type.Char, true, Type.Void), new FunctionValue("In.Char")), "In.Char");
			addIdent(new Identifier(Type.Function(Type.ShortInt, true, Type.Void), new FunctionValue("In.ShortInt")), "In.ShortInt");
			addIdent(new Identifier(Type.Function(Type.Integer, true, Type.Void), new FunctionValue("In.Integer")), "In.Integer");
			addIdent(new Identifier(Type.Function(Type.Integer, true, Type.Void), new FunctionValue("In.Int")), "In.Int");
			addIdent(new Identifier(Type.Function(Type.LongInt, true, Type.Void), new FunctionValue("In.LongInt")), "In.LongInt");
			addIdent(new Identifier(Type.Function(Type.Real, true, Type.Void), new FunctionValue("In.Real")), "In.Real");
			addIdent(new Identifier(Type.Function(Type.LongReal, true, Type.Void), new FunctionValue("In.LongReal")), "In.LongReal");
			addIdent(new Identifier(Type.VoidFunction, new FunctionValue("In.Open")), "In.Open");
		}
		else if(name.equals("Out")) {
			addIdent(new Identifier(Type.VoidFunction, new FunctionValue("Out.Ln")), "Out.Ln");
			addIdent(new Identifier(Type.Function(Type.Char,     false, Type.Void), new FunctionValue("Out.Char")),     "Out.Char");
			addIdent(new Identifier(Type.Function(Type.String,   false, Type.Void), new FunctionValue("Out.String")),   "Out.String");
			
			addIdent(new Identifier(Type.Function(Type.ShortInt, Type.Integer, Type.Void), new FunctionValue("Out.ShortInt")), "Out.ShortInt");
			addIdent(new Identifier(Type.Function(Type.Integer,  Type.Integer, Type.Void), new FunctionValue("Out.Int")),      "Out.Int");
			addIdent(new Identifier(Type.Function(Type.LongInt,  Type.Integer, Type.Void), new FunctionValue("Out.LongInt")),  "Out.LongInt");
			addIdent(new Identifier(Type.Function(Type.Real,     Type.Integer, Type.Void), new FunctionValue("Out.Real")),     "Out.Real");
			addIdent(new Identifier(Type.Function(Type.LongReal, Type.Integer, Type.Void), new FunctionValue("Out.LongReal")), "Out.LongReal");
			
			addIdent(new Identifier(Type.Function(Type.String, Type.ShortInt, Type.Void), new FunctionValue("Out.ShortIntFormat")), "Out.ShortIntFormat");
			addIdent(new Identifier(Type.Function(Type.String, Type.Integer,  Type.Void), new FunctionValue("Out.IntFormat")),      "Out.IntFormat");
			addIdent(new Identifier(Type.Function(Type.String, Type.LongInt,  Type.Void), new FunctionValue("Out.LongIntFormat")),  "Out.LongIntFormat");
			addIdent(new Identifier(Type.Function(Type.String, Type.Real,     Type.Void), new FunctionValue("Out.RealFormat")),     "Out.RealFormat");
			addIdent(new Identifier(Type.Function(Type.String, Type.LongReal, Type.Void), new FunctionValue("Out.LongRealFormat")), "Out.LongRealFormat");
		}
		else if(name.equals("Math")) {
			addIdent(new Identifier(Type.Function(Type.Real), new FunctionValue("Math.PI")), "Math.PI");
			addIdent(new Identifier(Type.Function(Type.LongReal), new FunctionValue("Math.L_PI")), "Math.L_PI");
			addIdent(new Identifier(Type.Function(Type.Real), new FunctionValue("Math.E")), "Math.E");
			addIdent(new Identifier(Type.Function(Type.LongReal), new FunctionValue("Math.L_E")), "Math.L_E");
			addIdent(new Identifier(Type.Function(Type.Real, false, Type.Real), new FunctionValue("Math.Exp")), "Math.Exp");
			addIdent(new Identifier(Type.Function(Type.Real, false, Type.Real), new FunctionValue("Math.Ln")), "Math.Ln");
			addIdent(new Identifier(Type.Function(Type.Real, false, Type.Real), new FunctionValue("Math.Sin")), "Math.Sin");
			addIdent(new Identifier(Type.Function(Type.Real, false, Type.Real), new FunctionValue("Math.Cos")), "Math.Cos");
			addIdent(new Identifier(Type.Function(Type.Real, false, Type.Real), new FunctionValue("Math.Atan")), "Math.Atan");
			addIdent(new Identifier(Type.Function(Type.Real, Type.Real, Type.Real), new FunctionValue("Math.Atan2")), "Math.Atan2");
			addIdent(new Identifier(Type.Function(Type.LongReal, false, Type.LongReal), new FunctionValue("Math.LongExp")), "Math.LongExp");
			addIdent(new Identifier(Type.Function(Type.LongReal, false, Type.LongReal), new FunctionValue("Math.LongLn")), "Math.LongLn");
			addIdent(new Identifier(Type.Function(Type.LongReal, false, Type.LongReal), new FunctionValue("Math.LongSin")), "Math.LongSin");
			addIdent(new Identifier(Type.Function(Type.LongReal, false, Type.LongReal), new FunctionValue("Math.LongCos")), "Math.LongCos");
			addIdent(new Identifier(Type.Function(Type.LongReal, false, Type.LongReal), new FunctionValue("Math.LongAtan")), "Math.LongAtan");
			addIdent(new Identifier(Type.Function(Type.LongReal, Type.LongReal, Type.LongReal), new FunctionValue("Math.LongAtan2")), "Math.LongAtan2");
		}
		else
			throw new Error(Error.COMPILEERROR, "Module \"" + name + "\" not found");
	}
}