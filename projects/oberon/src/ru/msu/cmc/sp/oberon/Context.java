package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Stack;
import java.util.Map.Entry;

import antlr.SemanticException;
import antlr.Token;

public class Context {
	public ArrayList<Variable> variables;
	public int mainId;
	public HashMap<String, Variable> variablesByName;
	public HashMap<String, Type> typesByName;
	public String fileName;
	
	protected Stack<CodeBlock> codeBlocks; // code blocks
	protected Stack<String> blockNames;    // their dotted names
	protected Stack<Integer> blockBegins;  // indexes of begins of begin..end block
	protected HashMap<String, String> moduleRenames; // (RenamedTo, RenamedFrom) 
	
	public Context() {
		variables = new ArrayList<Variable>();
		variablesByName = new HashMap<String, Variable>();
		typesByName = new HashMap<String, Type>();
		//stack = new Stack<TypedValue>();
		codeBlocks = new Stack<CodeBlock>();
		blockNames = new Stack<String>();
		blockBegins = new Stack<Integer>();
		moduleRenames = new HashMap<String, String>();
		
		// Init default types
		typesByName.put("BOOLEAN", Type.BooleanType);
		typesByName.put("INTEGER", Type.IntegerType);
		typesByName.put("LONGINT", Type.LongIntType);
		typesByName.put("SHORTINT", Type.ShortIntType);
		typesByName.put("CHAR", Type.CharType);
		typesByName.put("STRING", Type.StringType);
		typesByName.put("REAL", Type.RealType);
		typesByName.put("LONGREAL", Type.LongRealType);
		
		// Init default functions
		
		try {
			AddVariable(new Variable("LONG",   Type.CreateInvokeType(2, Type.LongRealType, true),  "LONG",   0, 0, true));
			AddVariable(new Variable("SHORT",  Type.CreateInvokeType(1, Type.LongRealType, false), "SHORT",  0, 0, true));
			AddVariable(new Variable("ASH",    Type.CreateInvokeType(2, Type.LongIntType,  true),  "ASH",    0, 0, true));
			AddVariable(new Variable("LSH",    Type.CreateInvokeType(1, Type.LongIntType,  true),  "LSH",    0, 0, true));
			AddVariable(new Variable("CHR",    Type.CreateInvokeType(1, Type.LongIntType,  false), "CHR",    0, 0, true));
			AddVariable(new Variable("ORD",    Type.CreateInvokeType(1, Type.CharType,     false), "ORD",    0, 0, true));
			AddVariable(new Variable("ENTIER", Type.CreateInvokeType(1, Type.LongRealType, false), "ENTIER", 0, 0, true));
			AddVariable(new Variable("LEN",    Type.CreateInvokeType(1, Type.VoidType,     false), "LEN",    0, 0, true));
		} catch (SemanticException e) {}
	}
	
	/*
	protected static int tempVarCount = 0;
	protected String newTempVar() {
		return "temp" + tempVarCount++;
	}*/
	
	public String toString() {
		StringBuffer result = new StringBuffer("");
		String newline = System.getProperty("line.separator");
		result.append("TYPE" + newline);
		for(Iterator<Entry<String,Type>> i = typesByName.entrySet().iterator(); i.hasNext();) {
			Entry<String,Type> entry = i.next();
			result.append(entry.getKey() + " = " + entry.getValue().toString() + newline);
		}
		result.append(newline + "VAR" + newline);
		for(Iterator<Variable> i = variables.iterator(); i.hasNext();) {
			Variable var = i.next();
			result.append(var.toString() + newline);
		}
		return result.toString();
	}
	
	public CodeBlock codeBlock() {
		return codeBlocks.lastElement();
	}
	public String blockName() {
		return blockNames.lastElement();
	}
	
	public void AddVariable(Variable var) throws SemanticException {
		if(blockNames.size() != 0)
			var.name = blockName() + "." + var.name;
		// Forward Proc Decl
		if(variablesByName.containsKey(var.name) && variablesByName.get(var.name).value().value == null && 
				var.type.type == BuiltInType.PROCEDURE && var.type.equals(variablesByName.get(var.name).type)) {
			var.id = variablesByName.get(var.name).id;
			variables.set(variablesByName.get(var.name).id, var);
			variablesByName.remove(var.name);
			variablesByName.put(var.name, var);
		}
		else if(variablesByName.containsKey(var.name)) 
			throw new SemanticException("Variable redefinition");
		else
		{
			var.id = variables.size();
			variables.add(var);
			variablesByName.put(var.name, var);
			if(codeBlocks.size() != 0)
				codeBlock().locals.add(var);
		}
	}
	/*
	public Variable AddTempVariable(Variable var) throws SemanticException {
		var.name = newTempVar();
		AddVariable(var);
		return var;
	}*/
	
	public void AddType(Type type) throws SemanticException {
		type.name = blockName() + "." + type.name;
		if(typesByName.containsKey(type.name)) 
			throw new SemanticException("Type redefinition");
		typesByName.put(type.name, type);
	}
	
	public void ImportModule(Token nameToken, Token importAsToken) throws SemanticException {
		//AddVariable(new Variable(importAsToken, BuiltInType.MODULE, null, true));
		moduleRenames.put(importAsToken.getText(), nameToken.getText());
		if(nameToken.getText().equals("In"))
		{
			AddVariable(new Variable("In.Open",      Type.CreateInvokeType(0, Type.VoidType,     true),  "In.Open",       0, 0, true));
			AddVariable(new Variable("In.Char",      Type.CreateInvokeType(1, Type.CharType,     true),  "In.Char",      0, 0, true));
			AddVariable(new Variable("In.ShortInt",  Type.CreateInvokeType(1, Type.ShortIntType, true),  "In.ShortInt",  0, 0, true));
			AddVariable(new Variable("In.Int",       Type.CreateInvokeType(1, Type.IntegerType,  true),  "In.Int",       0, 0, true));
			AddVariable(new Variable("In.LongInt",   Type.CreateInvokeType(1, Type.LongIntType,  true),  "In.LongInt",   0, 0, true));
			AddVariable(new Variable("In.Real",      Type.CreateInvokeType(1, Type.RealType,     true),  "In.Real",      0, 0, true));
			AddVariable(new Variable("In.LongReal",  Type.CreateInvokeType(1, Type.LongRealType, true),  "In.LongReal",  0, 0, true));
			AddVariable(new Variable("In.String",    Type.CreateInvokeType(1, Type.StringType,   true),  "In.String",    0, 0, true));
		}
		if(nameToken.getText().equals("Out"))
		{
			AddVariable(new Variable("Out.Out",      Type.CreateInvokeType(1, Type.LongRealType, false), "Out.Out",      0, 0, true));
			AddVariable(new Variable("Out.Char",     Type.CreateInvokeType(1, Type.CharType,     false), "Out.Char",     0, 0, true));
			AddVariable(new Variable("Out.String",   Type.CreateInvokeType(1, Type.StringType,   false), "Out.String",   0, 0, true));
			AddVariable(new Variable("Out.Ln",       Type.CreateInvokeType(0, Type.VoidType,     false), "Out.Ln",       0, 0, true));

			AddVariable(new Variable("Out.ShortInt", Type.CreateInvokeType(2, Type.ShortIntType, Type.IntegerType), "Out.ShortInt", 0, 0, true));
			AddVariable(new Variable("Out.Int",      Type.CreateInvokeType(2, Type.IntegerType,  Type.IntegerType), "Out.Int",      0, 0, true));
			AddVariable(new Variable("Out.Integer",  Type.CreateInvokeType(2, Type.IntegerType,  Type.IntegerType), "Out.Integer",  0, 0, true));
			AddVariable(new Variable("Out.LongInt",  Type.CreateInvokeType(2, Type.LongIntType,  Type.IntegerType), "Out.LongInt",  0, 0, true));
			AddVariable(new Variable("Out.Real",     Type.CreateInvokeType(2, Type.RealType,     Type.IntegerType), "Out.Real",     0, 0, true));
			AddVariable(new Variable("Out.LongReal", Type.CreateInvokeType(2, Type.LongRealType, Type.IntegerType), "Out.LongReal", 0, 0, true));

			AddVariable(new Variable("Out.ShortIntFormat", Type.CreateInvokeType(2, Type.StringType, Type.ShortIntType), "Out.ShortIntFormat", 0, 0, true));
			AddVariable(new Variable("Out.IntFormat",      Type.CreateInvokeType(2, Type.StringType, Type.IntegerType ), "Out.IntFormat",      0, 0, true));
			AddVariable(new Variable("Out.IntegerFormat",  Type.CreateInvokeType(2, Type.StringType, Type.IntegerType ), "Out.IntegerFormat",  0, 0, true));
			AddVariable(new Variable("Out.LongIntFormat",  Type.CreateInvokeType(2, Type.StringType, Type.LongIntType ), "Out.LongIntFormat",  0, 0, true));
			AddVariable(new Variable("Out.RealFormat",     Type.CreateInvokeType(2, Type.StringType, Type.RealType    ), "Out.RealFormat",     0, 0, true));
			AddVariable(new Variable("Out.LongRealFormat", Type.CreateInvokeType(2, Type.StringType, Type.LongRealType), "Out.LongRealFormat", 0, 0, true));
		}
		if(nameToken.getText().equals("Math"))
		{
			AddVariable(new Variable("Math.PI",        Type.CreateInvokeType(0, Type.LongRealType,  false), "Math.PI",        0, 0, true));
			AddVariable(new Variable("Math.L_PI",      Type.CreateInvokeType(0, Type.LongRealType,  false), "Math.L_PI",      0, 0, true));
			AddVariable(new Variable("Math.E",         Type.CreateInvokeType(0, Type.LongRealType,  false), "Math.E",         0, 0, true));
			AddVariable(new Variable("Math.L_E",       Type.CreateInvokeType(0, Type.LongRealType,  false), "Math.L_E",       0, 0, true));
			AddVariable(new Variable("Math.Exp",       Type.CreateInvokeType(1, Type.RealType,      false), "Math.Exp",       0, 0, true));
			AddVariable(new Variable("Math.Ln",        Type.CreateInvokeType(1, Type.RealType,      false), "Math.Ln",        0, 0, true));
			AddVariable(new Variable("Math.Sin",       Type.CreateInvokeType(1, Type.RealType,      false), "Math.Sin",       0, 0, true));
			AddVariable(new Variable("Math.Cos",       Type.CreateInvokeType(1, Type.RealType,      false), "Math.Cos",       0, 0, true));
			AddVariable(new Variable("Math.Atan",      Type.CreateInvokeType(1, Type.RealType,      false), "Math.Atan",      0, 0, true));
			AddVariable(new Variable("Math.Atan2",     Type.CreateInvokeType(2, Type.RealType,      false), "Math.Atan2",     0, 0, true));
			AddVariable(new Variable("Math.LongExp",   Type.CreateInvokeType(1, Type.LongRealType,  false), "Math.LongExp",   0, 0, true));
			AddVariable(new Variable("Math.LongLn",    Type.CreateInvokeType(1, Type.LongRealType,  false), "Math.LongLn",    0, 0, true));
			AddVariable(new Variable("Math.LongSin",   Type.CreateInvokeType(1, Type.LongRealType,  false), "Math.LongSin",   0, 0, true));
			AddVariable(new Variable("Math.LongCos",   Type.CreateInvokeType(1, Type.LongRealType,  false), "Math.LongCos",   0, 0, true));
			AddVariable(new Variable("Math.LongAtan",  Type.CreateInvokeType(1, Type.LongRealType,  false), "Math.LongAtan",  0, 0, true));
			AddVariable(new Variable("Math.LongAtan2", Type.CreateInvokeType(2, Type.LongRealType,  false), "Math.LongAtan2", 0, 0, true));
			AddVariable(new Variable("Math.Random",    Type.CreateInvokeType(1, Type.LongRealType,  false), "Math.Random",    0, 0, true));
		}
	}
	public void ImportModule(Token nameToken) throws SemanticException {
		ImportModule(nameToken, nameToken);
	}
	
	public void EnterBlock(Variable block) {
		String UndottedName = block.name;
		if(UndottedName.contains("."))
			UndottedName = UndottedName.substring(UndottedName.lastIndexOf(".") + 1);
		if(blockNames.size() != 0)
			blockNames.push(blockName() + "." + UndottedName);
		else
			blockNames.push(UndottedName);
		codeBlocks.push((CodeBlock)block.value().value);
	}
	public void LeaveBlock() {
		blockNames.pop();
		codeBlocks.pop();
	}
	
	public void EnterBeginEndBlock(int jmpToEndPosition) {
		blockBegins.push(jmpToEndPosition);
	}
	public void LeaveBeginEndBlock() {
		blockBegins.pop();
	}
	public Integer CurrentBlockJmpToEndPosition() {
		return blockBegins.lastElement();
	}
	
	public Object scopedGet(String name, HashMap<String, ?> from) throws SemanticException {
		if(name.contains(".")) { // is Module.Name
			String moduleName = name.substring(0, name.indexOf("."));
			if(moduleRenames.containsKey(moduleName))
				name = moduleRenames.get(moduleName) + name.substring(name.indexOf("."));
			if(from.containsKey(name))
				return from.get(name);
			else
				throw new SemanticException("Identifier \"" + name + "\" not found");
		}
		for(int i = blockNames.size() - 1; i >= 0; i--) {
			String tmp = blockNames.get(i) + "." + name;
			if(from.containsKey(tmp))
				return from.get(tmp);
		}
		if(from.containsKey(name))
			return from.get(name);
		throw new SemanticException("Identifier \"" + name + "\" not found");
	}
	
	public Type getType(String typeName) throws SemanticException {
		return (Type) scopedGet(typeName, typesByName);
	}
	
	public int getVariableId(String varName) throws SemanticException {
		return ((Variable) scopedGet(varName, variablesByName)).id;
	}
	
}
