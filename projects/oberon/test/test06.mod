MODULE scalaro;
IMPORT In, Out;
CONST MAXSIZE = 1024;
TYPE tarr = ARRAY MAXSIZE OF LONGREAL;
VAR n, i : INTEGER;
  v1, v2 : tarr;

PROCEDURE scalar(VAR v1, v2 : tarr; n : INTEGER) : LONGREAL;
  VAR res : LONGREAL;
    i : INTEGER;
BEGIN
  res := 0;
  FOR i := 0 TO n - 1 DO
    res := res + v1[i]*v2[i];
  END;
  RETURN res;
END scalar;

BEGIN
  In.Int(n);
  IF (n <= 0) OR (n > MAXSIZE) THEN
    Out.String('value of n (');
    Out.Int(n);
    Out.String(') is invalid');
    Out.Ln;
    HALT(20);
  END;
  FOR i := 0 TO n - 1 DO
    In.LongReal(v1[i]);
  END;
  FOR i := 0 TO n - 1 DO
    In.LongReal(v2[i]);
  END;
  Out.LongReal(scalar(v1, v2, n));
END scalaro.
