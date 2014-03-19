package ru.msu.cmc.sp.oberon;

import java.io.*;
import java.util.ArrayList;

import ru.msu.cmc.sp.oberon.AsmStatement.AsmStatementType;

import antlr.RecognitionException;
import antlr.TokenStreamException;

public class Oberon{
	public static boolean AsmDump = false;
	public static String DumpFile = null;
	public static boolean CheckOnly = false;
	public static boolean DefsOnly = false;
	public static boolean UsesOnly = false;
	public static boolean Debug = false;
	public static String IdentName = "";
	public static int IdentLine = -1;
	public static int IdentCol = -1;
	public static boolean runned = false;

	public static void main(String[] args){
		Oberon2Parser parser;
		String InFileName = args[args.length - 1];
		try {
			parser = new Oberon2Parser(new Oberon2Lexer(new BufferedReader(new FileReader(InFileName))));
		} catch (FileNotFoundException e1) {
			System.out.print(e1);
			System.exit(1);
			return;
		}
		
		for(int i = 0; i < args.length - 1; i++)
			if(args[i].equals("--check-only"))
				CheckOnly = true;
			else if(args[i].equals("--defs"))
				DefsOnly = true;
			else if(args[i].equals("--dump") && i != args.length - 2)
			{
				AsmDump = true;
				i++;
				DumpFile = args[i];
			}
			else if(args[i].equals("--debug"))
				Debug = true;
			else if(args[i].equals("--uses") && i != args.length - 2)
			{
				i++;
				String param = args[i]; 
				if(param.indexOf(":") != -1)
				{
					IdentLine = Integer.parseInt(param.substring(0, param.indexOf(":")));
					param = param.substring(param.indexOf(":") + 1);
					if(param.indexOf(":") != -1)
					{
						IdentCol = Integer.parseInt(param.substring(0, param.indexOf(":")));
						param = param.substring(param.indexOf(":") + 1);
					}
				}
				IdentName = param;
				UsesOnly = true;
			}
		
		
		try {
			parser.Parse(InFileName);

			if(AsmDump) {
				BufferedWriter f = new BufferedWriter(new FileWriter(DumpFile));
				f.write(parser.context.toString());
				f.close();
			}
			if(DefsOnly) {
				int[] usages = new int[parser.context.variables.size()];
				for(int i = 0; i < parser.context.variables.size(); i++) {
					Variable v = parser.context.variables.get(i);
					if(v.type.type == BuiltInType.PROCEDURE && !v.type.isInvoke()) {
						CodeBlock block = (CodeBlock)v.value().value;
						for(int j = 0; j < block.statements.size(); j++) {
							if(block.statements.get(j).type == AsmStatementType.CALL || block.statements.get(j).type == AsmStatementType.PUSHVAR) {
								usages[(Integer)block.statements.get(j).f]++;
							}
						}
					}
				}
				for(int i = 0; i < parser.context.variables.size(); i++) {
					Variable v = parser.context.variables.get(i);
					if(v.type.type != BuiltInType.PROCEDURE || (v.type.type == BuiltInType.PROCEDURE && !v.type.isInvoke())) {
						String tmp = v.definitionLine + ":" + v.definitionCol + ":" + v.name + ":";
						if(v.type.type == BuiltInType.PROCEDURE)
							tmp += "P";
						else if(v.isConst)
							tmp += "C";
						else
							tmp += "V";
						tmp += ":" + usages[v.id];
						System.out.println(tmp);
					}
				}
			}
			if(UsesOnly) {
				Variable needed = null;
				int count = 0;
				for(int i = 0; i < parser.context.variables.size(); i++) {
					Variable v = parser.context.variables.get(i);
					String lastName = v.name;
					if(lastName.contains("."))
						lastName = lastName.substring(lastName.lastIndexOf(".") + 1);
					if(IdentName.equals(lastName) && (v.definitionLine == IdentLine || IdentLine == -1) && 
						(v.definitionCol == IdentCol || IdentCol == -1)) {
						count++;
						needed = v;
					}
				}
				if(count == 1) {
					for(int i = 0; i < parser.context.variables.size(); i++) {
						Variable v = parser.context.variables.get(i);
						if(v.type.type == BuiltInType.PROCEDURE && !v.type.isInvoke()) {
							CodeBlock block = (CodeBlock)v.value().value;
							for(int j = 0; j < block.statements.size(); j++) {
								if((block.statements.get(j).type == AsmStatementType.CALL || block.statements.get(j).type == AsmStatementType.PUSHVAR) &&
										((Integer)block.statements.get(j).f) == needed.id) {
									AsmStatement asm = block.statements.get(j);
									System.out.println(InFileName + ":" + asm.sourceLine + ":" + asm.sourceCol);
								}
							}
						}
					}
				}
			}
			
			if(!CheckOnly && !DefsOnly && !UsesOnly) {
				VirtualMachine vm = new VirtualMachine(parser.context);
				runned = true;
				vm.Run();
			}
		} catch (RuntimeError e) {
			System.err.println(e.getMessage());
			System.exit((runned)?(e.error.code()):(100));
		} catch (Exception e) {
			if(Debug)
				e.printStackTrace(System.err);
			else
				System.err.println(e.getMessage());
			System.exit(100);
		}
		return;
	}
}
