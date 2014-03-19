MODULE breaks2;
IMPORT In, Out;

VAR i: INTEGER;

PROCEDURE ae(): STRING;
BEGIN;
  LOOP 
     In.Int(i); 
     IF i < 0 THEN EXIT END; 
     IF i = 0 THEN CONTINUE END; 
     Out.Int(i + 1)
  END;
  RETURN "Ae!";
END ae;

BEGIN
  Out.String(ae);
END breaks2.
