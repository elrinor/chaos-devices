package ru.msu.cmc.sp.oberon;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Input {
	public static BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
	public static String[] buf;
	public static int bufpos;
	
	
	public static String readString() throws IOException {
		String result = peekString();
		bufpos++;
		return result;
	}
	public static String peekString() throws IOException {
		if(buf == null || bufpos == buf.length)
		{
			buf = reader.readLine().split("( |\t)+");
			bufpos = 0;
		}
		return buf[bufpos];
	}
	public static short readShort() throws NumberFormatException, IOException, RuntimeError {
		long v = readLong();
		Type.CheckShortBounds(v);
		return (short)v;
	}
	public static int readInt() throws NumberFormatException, IOException, RuntimeError {
		long v = readLong();
		Type.CheckIntBounds(v);
		return (int)v;
	}
	public static long readLong() throws NumberFormatException, IOException, RuntimeError {
		double dv = Double.parseDouble(peekString());
		Type.CheckLongBounds(dv);
		return Long.parseLong(readString());
	}
	public static float readFloat() throws NumberFormatException, IOException {
		return Float.parseFloat(readString());
	}
	public static double readDouble() throws NumberFormatException, IOException {
		return Double.parseDouble(readString());
	}
	public static char readChar() throws IOException {
		peekString();
		char result = buf[bufpos].charAt(0);
		buf[bufpos] = buf[bufpos].substring(1);
		if(buf[bufpos].length() == 0)
			bufpos++;
		return result;
	}


}
