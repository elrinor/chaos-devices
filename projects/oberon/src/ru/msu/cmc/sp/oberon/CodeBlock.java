package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;

public class CodeBlock {
	ArrayList<AsmStatement> statements;
	ArrayList<Variable> locals;
	
	public CodeBlock() {
		statements = new ArrayList<AsmStatement>();
		locals = new ArrayList<Variable>();
	}
	/*
	public CodeBlock(String invoke) {
		this();
		//statements.add(AsmStatement.Invoke(invoke));
		statements.add(AsmStatement.Ret());
	}*/
	
	public String toString() {
		StringBuffer result = new StringBuffer();
		String newline = System.getProperty("line.separator");
		result.append("{" + newline);
		result.append("  LOCALS ");
		for(int i = 0; i < locals.size(); i++)
		{
			result.append(locals.get(i).id);
			if(i + 1 < locals.size())
				result.append(", ");
		}
		result.append(";" + newline);
		for(int i = 0; i < statements.size(); i++)
			result.append("  " + i + ": " + statements.get(i).toString() + ";" + newline);
		result.append("}");
		return result.toString();
	}
	
	public void removeLastStatement() {
		statements.remove(statements.size() - 1);
	}
}
