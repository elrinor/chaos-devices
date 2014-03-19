(* -*- mode: oberon -*- *)

MODULE ack_o;
IMPORT In, Out;

VAR a, b : LONGINT;

PROCEDURE ack(k, m : LONGINT) : LONGINT;
BEGIN
  IF k = 0 THEN
    RETURN m + 1
  ELSIF m = 0 THEN
    RETURN ack(k - 1, 1)
  ELSE
    RETURN ack(k - 1, ack(k, m - 1))
  END;
END ack;

BEGIN
  In.LongInt(a);
  In.LongInt(b);
  Out.Int(ack(a, b));
  Out.Ln
END ack_o.