(* TEST1 *)
MODULE test7;
IMPORT In, Out;

PROCEDURE DoIt;
VAR s : STRING;
BEGIN
  REPEAT
    Out.String("Enter your name: ");
    In.String(s);
    IF s # "q" THEN
      Out.String("Hello, " + s);
      Out.Ln;
      Out.Ln;
    END;
  UNTIL s = "q";
END DoIt;

PROCEDURE DoIt2;
BEGIN
  DoIt;
END DoIt2;

BEGIN
  DoIt2;
END test7.
