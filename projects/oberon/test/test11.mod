(* TEST1 *)
MODULE power;
IMPORT In, Out;

VAR a,b,x:LONGINT;
    i:INTEGER;
BEGIN;
In.LongInt(a);
In.LongInt(b);
IF (b=0) THEN
  Out.String('01');
  HALT;
END;

x:=a MOD 100;
FOR i:=1 TO (b-1) DO
  x:=(x*a) MOD 100;
END;

Out.Ln;
Out.Integer(a);
Out.String('^');
Out.Integer(b);
Out.String(' MOD 100 = ');
IF x<10 THEN Out.Char('0') END;
Out.LongInt(x);
Out.Ln;
END power.
