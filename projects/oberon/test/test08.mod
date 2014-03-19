(* -*- mode: oberon -*- *)

MODULE q8_o;
IMPORT In, Out;

VAR
   up, down : ARRAY 15 OF BOOLEAN;
   rows     : ARRAY 8 OF BOOLEAN;
   x        : ARRAY 8 OF INTEGER;
   i        : INTEGER;

   PROCEDURE queens(c : INTEGER);
   VAR r : INTEGER;
      PROCEDURE print;
      VAR c : INTEGER;
      BEGIN
         FOR c := 0 TO 7 DO
            Out.Int(1 + x[c], 0);
            Out.String(' ');
         END;
      Out.Ln
      END print;
   BEGIN
      FOR r := 0 TO 7 DO
         IF rows[r] & up[r-c+7] & down[r+c] THEN
            rows[r] := FALSE;
            up[r - c + 7] := FALSE;
            down[r + c] := FALSE;
            x[c] := r;
            IF c = 7 THEN print ELSE queens(c + 1) END;
            rows[r] := TRUE;
            up[r - c + 7] := TRUE;
            down[r + c] := TRUE;
         END
      END
   END queens;

BEGIN
   FOR i := 0 TO 14 DO
      up[i] := TRUE;
      down[i] := TRUE
   END;
   FOR i := 0 TO 7 DO
      rows[i] := TRUE;
   END;
   queens(0);
END q8_o.
