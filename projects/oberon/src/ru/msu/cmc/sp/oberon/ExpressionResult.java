package ru.msu.cmc.sp.oberon;

public class ExpressionResult {
	public boolean isConst;
	public TypedValue value;
	public ExpressionResult(boolean isConst, TypedValue value) {
		this.isConst = isConst;
		this.value = value;
	}
}
