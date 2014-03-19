package ru.msu.cmc.sp.oberon;

import java.io.BufferedReader;
import java.io.FileReader;

import antlr.TokenStreamRecognitionException;

public class Oberon {

	private static String fileName;
	private static Oberon2Parser parser;
	private static boolean debug = false;
	private static boolean checkOnly = false;
	private static boolean defs = false;
	
	private static void Parse() {
		try {
			parser.module();
		} catch (TokenStreamRecognitionException e) {
			System.err.println(fileName + ":" + e.recog.line + ":" + e.recog.column + ":" + e.getMessage());
			if(debug)
				e.printStackTrace(System.out);
			System.exit(100);
		} catch (Exception e) {
			try {
				System.err.println(fileName + ":" + parser.LT(0).getLine() + ":" + parser.LT(0).getColumn() + ":" + e.getMessage());
			} catch (Exception e1) {
				System.err.println(fileName + ":0:0:" + e.getMessage());
			}
			if(debug)
				e.printStackTrace(System.out);
			if(e instanceof Error)
				System.exit(((Error)e).errorCode());
			else
				System.exit(100);
		}
	}
	
	public static void main(String[] args) {
		fileName = args[args.length - 1];
		for(int i = 0; i < args.length - 1; i++) {
			if(args[i].equals("--check-only"))
				checkOnly = true;
			else if(args[i].equals("--defs"))
				defs = true;
			else if(args[i].equals("--uses")) {
				Module.uses = true;
				i++;
				String[] a = args[i].split("\\:");
				if(a.length == 1)
					Module.usesName = a[0];
				else if(a.length == 2) {
					Module.usesName = a[1];
					Module.usesLine = Integer.parseInt(a[0]);
				}
				else if(a.length == 3) {
					Module.usesName = a[2];
					Module.usesCol = Integer.parseInt(a[1]);
					Module.usesLine = Integer.parseInt(a[0]);
				}
			}
		}
		try {
			parser = new Oberon2Parser(new Oberon2Scanner(new BufferedReader(new FileReader(fileName))));
		} catch (Exception e) {
			System.err.println("Error while opening input file: " + e.getMessage());
			System.exit(1);
		}
		Parse();
		
		if(defs) {
			for(int i = 0; i < Module.idents.size(); i++)
				if(!(Module.idents.get(i).defCol == 0 && Module.idents.get(i).defLine == 0)) {
					System.out.print(Module.idents.get(i).defLine + ":");
					System.out.print(Module.idents.get(i).defCol + ":");
					System.out.print(Module.idents.get(i).name + ":");
					if(Module.idents.get(i).getType().typeId() == Type.FUNCTION)
						System.out.print("P:");
					else
						System.out.print("V:");
					System.out.println(Module.idents.get(i).usages);
				}
		}
		if(!checkOnly && !defs && !Module.uses) {
			try {
				Module.main.run();
			} catch (Error.Halt e) {
				System.exit(e.getCode());
			} catch (Error e) {
				if(debug)
					e.printStackTrace(System.out);
				System.err.println(fileName + ":0:0:" + e.getMessage());
				System.exit(e.errorCode());
			} catch (Exception e) {
				if(debug)
					e.printStackTrace(System.out);
				System.err.println(fileName + ":0:0:" + e.getMessage());
				System.exit(100);
			}
		}
	}
}





