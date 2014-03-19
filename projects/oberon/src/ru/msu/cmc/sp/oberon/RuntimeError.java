package ru.msu.cmc.sp.oberon;

import antlr.SemanticException;

public class RuntimeError extends SemanticException {
	private static final long serialVersionUID = 1L;
	public Error error;
	public String detailedErrorMessage;

	public RuntimeError(RuntimeError error, String message) {
		super(message);
		this.error = error.error;
	}
	
	public RuntimeError(Error error, String message) {
		super(error.message());
		detailedErrorMessage = message;
		this.error = error;
	}

	public RuntimeError(Error error) {
		this(error, error.message());
	}
}
