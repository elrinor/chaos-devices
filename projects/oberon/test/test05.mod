MODULE test5;
IMPORT m:=Math, Out;

VAR s: STRING;
BEGIN
  Out.String("PI = ");
  Out.Real(m.PI);
  Out.Ln;
  Out.String("L_PI = ");
  Out.LongReal(m.L_PI);
  Out.Ln;
  Out.String("E = ");
  Out.Real(m.E);
  Out.Ln;
  Out.String("L_E = ");
  Out.LongReal(m.L_E);
  Out.Ln;
  Out.String("sin(PI/2) = ");
  Out.LongReal(m.Sin(m.PI / 2));
  Out.Ln;
  Out.String("cos(PI/2) = ");
  Out.LongReal(m.Cos(m.PI / 2));
  Out.Ln;
  Out.String("exp(1) = ");
  Out.LongReal(m.Exp(1));
  Out.Ln;
  Out.String("ln(E) = ");
  Out.LongReal(m.Ln(m.E));
  Out.Ln;
  Out.String("arctg(1) = ");
  Out.LongReal(m.Atan(1));
  Out.Ln;
  Out.String("arctg2(0, 1) = ");
  Out.LongReal(m.Atan2(0, 1));
  Out.Ln;
END test5.