MODULE test4;
IMPORT Out, In;

VAR i, n, s: LONGINT;
BEGIN
  In.LongInt(n);
  s := 0;
  IF n >= 1 THEN
    FOR i := 1 TO n DO INC(s, i); END;
  ELSE
    FOR i := 1 TO n BY -1 DO INC(s, i); END;
  END;
  Out.LongInt(s);
END test4.