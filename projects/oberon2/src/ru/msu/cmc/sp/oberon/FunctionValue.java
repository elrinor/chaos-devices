package ru.msu.cmc.sp.oberon;

import java.util.ArrayList;

public class FunctionValue {
	public ArrayList<Integer> locals;
	public Statement body;
	public boolean isDefault;
	public String name;
	public FunctionValue() {
		this.locals = new ArrayList<Integer>();
		this.isDefault = false;
	}
	public FunctionValue(ArrayList<Integer> locals, Statement body) {
		this.locals = locals;
		this.body = body;
		this.isDefault = false;
	}
	public FunctionValue(String name) {
		this.isDefault = true;
		this.name = name;
	}
}
