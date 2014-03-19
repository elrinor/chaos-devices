MODULE nice;
IMPORT In, Out, Math;

CONST size=64;
VAR a, a1:ARRAY size + 1, size + 1 OF SHORTINT;
    b:ARRAY size + 1, size + 1 OF CHAR;
                   
VAR dl,x,y,x1,y1,Chance,InitType:INTEGER;
    sx,sy:INTEGER;
    c:CHAR;
BEGIN;
FOR x:=1 TO size DIV 2 DO
  FOR y:=1 TO size DIV 2 DO a[x,y]:=1; END;
END;
  

FOR x:=size DIV 2+1 TO size DO
  FOR y:=size DIV 2+1 TO size DO a[x,y]:=1; END;
END;
dl:=size DIV 2;

REPEAT
dl:=dl DIV 2;
FOR x:=1 TO size DIV dl DO
  FOR y:=1 TO size DIV dl DO
    Chance:=0;
    IF (x)*dl+dl<=size THEN IF a[x*dl+dl,y*dl]=0 THEN INC(Chance); END; END;
    IF (x)*dl-dl>=1    THEN IF a[x*dl-dl,y*dl]=0 THEN INC(Chance); END; END;
    IF (y)*dl+dl<=size THEN IF a[x*dl,y*dl+dl]=0 THEN INC(Chance); END; END;
    IF (y)*dl-dl>=1    THEN IF a[x*dl,y*dl-dl]=0 THEN INC(Chance); END; END;
    IF (1+Math.Random(4))<=Chance THEN
      FOR x1:=x*dl-dl+1 TO x*dl DO
      FOR y1:=y*dl-dl+1 TO y*dl DO
        a1[x1,y1]:=0;
      END;          
      END;
    ELSE
      FOR x1:=x*dl-dl+1 TO x*dl DO
      FOR y1:=y*dl-dl+1 TO y*dl DO
        a1[x1,y1]:=1;
      END;
      END;
    END;
  END;
END;
a:=a1;
UNTIL dl=1;

FOR x:=1 TO size DO
  FOR y:=1 TO size DO
    IF a[x,y]=0 THEN
      b[x,y]:='#';
    END;
    IF a[x,y]=1 THEN
      b[x,y]:='=';
    END;
  END;
END;

sx:=0;
sy:=0;

FOR y:=1 TO size DO
  Out.Ln;
  FOR x:=1 TO size DO
    Out.Char(b[x,y]);
  END;
END;

END nice.
