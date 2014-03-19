MODULE breaks;
IMPORT In, Out;

VAR i, by, from, to : INTEGER;
  s: STRING;

PROCEDURE ae(): STRING;
BEGIN;
  Out.String("From: ");
  In.Int(from);
  Out.String("To: ");
  In.Int(to);
  Out.String("By: ");
  In.Int(by);
  FOR i := from TO to BY by DO
    Out.String("Enter q to leave: ");
    In.String(s);
    IF s = "q" THEN BREAK END;
    IF s = "c" THEN CONTINUE END;
    Out.String("i = ");
    Out.Int(i);
    Out.Ln;
  END;
  RETURN "Ae!";
END ae;

BEGIN
  Out.String(ae);
END breaks.
