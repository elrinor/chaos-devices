(* TEST2 *)
(**) (* (* *) *)
MODULE test2;
IMPORT Out;

PROCEDURE log2 (x: INTEGER): INTEGER; 
     VAR y: INTEGER; (*assuming x>0*) 
BEGIN 
     y := 0; WHILE x > 1 DO x := x DIV 2; y := y + 1 END; 
     RETURN y 
END log2;

VAR i : INTEGER;
BEGIN
  Out.String("Log2(16) = ");
  Out.Int(log2(16));
  Out.Ln;
END test2.
