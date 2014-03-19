MODULE chartest;
IMPORT o:=Out;

CONST c=97x;
      s="Asa! Asa da yo, asagohan o tabete, gakkou e iku yo!";
VAR
  s1: ARRAY 100 OF CHAR;
  s2: STRING;
BEGIN;
  o.Char(c);
  o.Char(98X);
  o.Char(99x);
  o.Char(100x);
  o.Ln;  
  s1 := s;
  s2 := s1;
  o.String(s);
  o.Ln;
  o.String(s1);
  o.Ln;
  o.String(s2);
  o.Ln;
END chartest.
