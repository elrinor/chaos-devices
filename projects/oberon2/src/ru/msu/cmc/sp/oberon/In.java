package ru.msu.cmc.sp.oberon;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

public class In {
	public static ArrayList<String> buffer = new ArrayList<String>();
	public static BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
	
	public static String String() throws Error{
		if(buffer.size() == 0) {
			try {
				String[] read = in.readLine().split("( |\t)+");
				for(int i = 0; i < read.length; i++)
					buffer.add(read[i]);
			} catch (IOException e) {
				throw new Error(Error.INPUTERROR);
			}
		}
		return buffer.remove(0);
	}
	
	public static Character Char() throws Error{
		String read = String();
		Character result = read.charAt(0);
		read = read.substring(1);
		if(read.length() > 0)
			buffer.add(0, read);
		return result;
	}
	
	public static Long Long() throws Error{
		return Long.parseLong(String());
	}
	public static Integer Int() throws Error{
		return (int)(long)Long();
	}
	public static Short Short() throws Error{
		return (short)(long)Long();
	}
	public static Double Double() throws Error{
		return Double.parseDouble(String());
	}
	public static Float Float() throws Error{
		return (float)(double)Double();
	}
}
