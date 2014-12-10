Program Tetris;
Uses Crt;
Type
    TPiece=record
                pType,rotate: byte;
                x,y:integer;
    end;
var
    Piece: TPiece;
    t:integer;
    c, LastKey:byte;
    fell:boolean;
const
     Score :integer=0;
{---------------------------------------------------------------------------}
procedure KillCursor;
begin
     gotoxy(2,2);
     mem[$B800:0083]:=0;
end;
{---------------------------------------------------------------------------}
procedure initscreen;
var x,y:integer;
begin
     randomize;
     t:=0;
     clrscr;
     textmode(C40);
     gotoxy(3,2);
     write ('SCORE:');

     KillCursor;
     mem[$B800:0000]:=201;
     for x:=1 to 9 do
         mem[$B800:2*x]:=205;
     mem[$B800:0018]:=187;
     mem[$B800:0060]:=201;
     for x:=31 to 38 do
         mem[$B800:2*x]:=205;
     mem[$B800:0078]:=187;

     for y:=1 to 23 do
     begin
          mem[$B800:80*y]:=186;
          mem[$B800:80*y+18]:=186;
          mem[$B800:80*y+60]:=186;
          mem[$B800:80*y+78]:=186;
     end;

     mem[$B800:1920]:=200;
     mem[$B800:1998]:=188;
     for x:=1 to 38 do
         mem[$B800:1920+2*x]:=205;

     mem[$B800:1858]:=200;
     mem[$B800:1900]:=188;
     for x:=10 to 29 do
         mem[$B800:1840+2*x]:=205;

     for x:=10 to 29 do
         for y:=0 to 22 do
         begin
             mem[$B800:80*y+2*x]:=219;
             mem[$B800:80*y+2*x+1]:=0;
         end
end;
{---------------------------------------------------------------------------}
procedure NewPiece;
begin
          Piece.ptype:=random(3)+1;
          Piece.rotate:=0;
          Piece.x:=20;
	  Piece.y:=0;
          Fell:=False;
end;
{---------------------------------------------------------------------------}
procedure Fallpiece;
var x,y, sty, count:integer;
begin
     for y:=1 to 22 do
     begin
          count:=0;
          for x:=10 to 29 do
              if (mem[$B800:80*y+2*x+1]=17) then inc (count);
          if (count=20) then
          begin
              for sty:=y downto 1 do
                  for x:=10 to 29 do
                      mem[$B800:80*sty+2*x+1]:=mem[$B800:80*(sty-1)+2*x+1];
              inc (Score);
          end;
     end;
     NewPiece;
end;
{---------------------------------------------------------------------------}
procedure DrawLine;
begin
     case (piece.rotate mod 2) of
          0:
          begin
               If ((mem[$B800:80*piece.y+2*piece.x-1]<>0) and (c<>0)) Then inc(Piece.x);
               If ((mem[$B800:80*piece.y+2*piece.x+5]<>0) and (c<>0)) Then dec(Piece.x);
               If ((mem[$B800:80*piece.y+2*piece.x+5]<>0) and (c<>0)) Then dec(Piece.x);
               If (((mem[$B800:80*(piece.y+1)+2*piece.x-1]<>0)or
                    (mem[$B800:80*(piece.y+1)+2*piece.x+1]<>0)or
                    (mem[$B800:80*(piece.y+1)+2*piece.x+3]<>0)or
                    (mem[$B800:80*(piece.y+1)+2*piece.x+5]<>0))and (c<>0)) Then
                               begin
                                    c:=17;
                                    fell:=True;
                               end;
               mem[$B800:80*piece.y+2*piece.x-1]:=c;
               mem[$B800:80*piece.y+2*piece.x+1]:=c;
               mem[$B800:80*piece.y+2*piece.x+3]:=c;
               mem[$B800:80*piece.y+2*piece.x+5]:=c;
          end;
          1:
          begin
               If (((mem[$B800:80*(piece.y-1)+2*piece.x+1]<>0)or
                    (mem[$B800:80*(piece.y+0)+2*piece.x+1]<>0)or
                    (mem[$B800:80*(piece.y+1)+2*piece.x+1]<>0)or
                    (mem[$B800:80*(piece.y+2)+2*piece.x+1]<>0))and
                    ((c<>0)and(LastKey=0))) Then inc (Piece.x);

               If (((mem[$B800:80*(piece.y-1)+2*piece.x+1]<>0)or
                    (mem[$B800:80*(piece.y+0)+2*piece.x+1]<>0)or
                    (mem[$B800:80*(piece.y+1)+2*piece.x+1]<>0)or
                    (mem[$B800:80*(piece.y+2)+2*piece.x+1]<>0))and
                    ((c<>0)and(LastKey=1))) Then dec (Piece.x);

               If ((mem[$B800:80*(piece.y+3)+2*piece.x+1]<>0) and (c<>0)) Then
				   begin
                                    c:=17;
                                    fell:=True;
                               end;

               mem[$B800:80*(piece.y-1)+2*piece.x+1]:=c;
               mem[$B800:80*(piece.y+0)+2*piece.x+1]:=c;
               mem[$B800:80*(piece.y+1)+2*piece.x+1]:=c;
               mem[$B800:80*(piece.y+2)+2*piece.x+1]:=c;
          end;
     end;
     If (Fell) Then FallPiece;
end;
{---------------------------------------------------------------------------}
procedure DrawBlock;
begin
     If (((mem[$B800:80*piece.y+2*piece.x+1]<>0)or
          (mem[$B800:80*(piece.y+1)+2*piece.x+1]<>0))and
          (c<>0)) then inc (Piece.x);
     If (((mem[$B800:80*piece.y+2*piece.x+3]<>0)or
	   (mem[$B800:80*(piece.y+1)+2*piece.x+3]<>0))and
          (c<>0)) then dec (Piece.x);
     If (((mem[$B800:80*(piece.y+2)+2*piece.x+1]<>0)or
          (mem[$B800:80*(piece.y+2)+2*piece.x+3]<>0))and
          (c<>0)) then
          begin
               Fell:=True;
               c:=17;
          end;
     mem[$B800:80*(piece.y+0)+2*piece.x+1]:=c;
     mem[$B800:80*(piece.y+1)+2*piece.x+1]:=c;
     mem[$B800:80*(piece.y+0)+2*piece.x+3]:=c;
     mem[$B800:80*(piece.y+1)+2*piece.x+3]:=c;
     If (Fell) Then FallPiece;
end;
{---------------------------------------------------------------------------}
procedure DrawCorner;
begin
     case Piece.Rotate of
	   0:begin
		   If ((mem[$B800:80*piece.y+2*piece.x-1]<>0)and(c<>0)) Then inc (Piece.x);
		   If ((mem[$B800:80*piece.y+2*piece.x+3]<>0)and(c<>0)) Then dec (Piece.x);
		   If (((mem[$B800:80*(piece.y+1)+2*piece.x-1]<>0)or
			 (mem[$B800:80*(piece.y+2)+2*piece.x+1]<>0)or
			 (mem[$B800:80*(piece.y+1)+2*piece.x+3]<>0))and (c<>0)) Then
				   begin
					 c:=17;
					 fell:=True;
				   end;
		   mem[$B800:80*piece.y+2*piece.x-1]:=c;
		   mem[$B800:80*piece.y+2*piece.x+1]:=c;
		   mem[$B800:80*piece.y+2*piece.x+3]:=c;
		   mem[$B800:80*(piece.y+1)+2*piece.x+1]:=c;
		   If (Fell) Then FallPiece;
	   end;
	   1:begin
		   If (((mem[$B800:80*(piece.y-1)+2*piece.x+1]<>0)or
			 (mem[$B800:80*piece.y+2*piece.x+1]<>0)or
			 (mem[$B800:80*(piece.y+1)+2*piece.x+1]<>0))and
			 (c<>0)) Then inc (Piece.x);
		   If (((mem[$B800:80*(piece.y-1)+2*piece.x+1]<>0)or
			 (mem[$B800:80*piece.y+2*piece.x+3]<>0)or
			 (mem[$B800:80*(piece.y+1)+2*piece.x+1]<>0))and
			 (c<>0)) Then dec (Piece.x);
		   If (((mem[$B800:80*(piece.y+2)+2*piece.x+1]<>0)or
			 (mem[$B800:80*(piece.y+1)+2*piece.x+3]<>0))and (c<>0)) Then
				   begin
					 c:=17;
					 fell:=True;
				   end;
		   mem[$B800:80*(piece.y-1)+2*piece.x+1]:=c;
		   mem[$B800:80*piece.y+2*piece.x+1]:=c;
		   mem[$B800:80*piece.y+2*piece.x+3]:=c;
		   mem[$B800:80*(piece.y+1)+2*piece.x+1]:=c;
		   If (Fell) Then FallPiece;
	   end;
	   2:begin
		   If (((mem[$B800:80*piece.y+2*piece.x-1]<>0)or
			 (mem[$B800:80*(piece.y-1)+2*piece.x+1]<>0))and
			 (c<>0)) Then inc (Piece.x);
		   If (((mem[$B800:80*piece.y+2*piece.x+3]<>0)or
			 (mem[$B800:80*(piece.y-1)+2*piece.x+1]<>0))and
			 (c<>0)) Then dec (Piece.x);
		   If (((mem[$B800:80*(piece.y+1)+2*piece.x-1]<>0)or
			 (mem[$B800:80*(piece.y+1)+2*piece.x+1]<>0)or
			 (mem[$B800:80*(piece.y+1)+2*piece.x+3]<>0))and (c<>0)) Then
				   begin
					 c:=17;
					 fell:=True;
				   end;
		   mem[$B800:80*piece.y+2*piece.x-1]:=c;
		   mem[$B800:80*piece.y+2*piece.x+1]:=c;
		   mem[$B800:80*piece.y+2*piece.x+3]:=c;
		   mem[$B800:80*(piece.y-1)+2*piece.x+1]:=c;
		   If (Fell) Then FallPiece;
	   end;
	   3:begin
		   If (((mem[$B800:80*(piece.y-1)+2*piece.x+1]<>0)or
			 (mem[$B800:80*piece.y+2*piece.x-1]<>0))and
			 (c<>0)and(LastKey=0)) Then inc (Piece.x);
		   If (((mem[$B800:80*(piece.y-1)+2*piece.x+1]<>0)or
			 (mem[$B800:80*piece.y+2*piece.x+1]<>0)or
			 (mem[$B800:80*(piece.y+1)+2*piece.x+1]<>0))and
			 (c<>0)) Then dec (Piece.x);
		   If (((mem[$B800:80*(piece.y+1)+2*piece.x-1]<>0)or
			 (mem[$B800:80*(piece.y+2)+2*piece.x+1]<>0))and (c<>0)) Then
				   begin
					 c:=17;
					 fell:=True;
				   end;
		   mem[$B800:80*(piece.y-1)+2*piece.x+1]:=c;
		   mem[$B800:80*piece.y+2*piece.x+1]:=c;
		   mem[$B800:80*piece.y+2*piece.x-1]:=c;
		   mem[$B800:80*(piece.y+1)+2*piece.x+1]:=c;
	   end;
	end;
	If (Fell) Then FallPiece;
end;
{---------------------------------------------------------------------------}
procedure DrawZigZag;
begin
end;
{---------------------------------------------------------------------------}
procedure DrawPiece(how:boolean);
begin
     If (how) Then c:=12
     else          c:=0;
     case (Piece.ptype) of
          1: DrawLine;
          2: DrawBlock;
          3: DrawCorner;
     end;
end;
{---------------------------------------------------------------------------}
begin
     initscreen;
     while (1=1) do
     begin
          NewPiece;
          While (piece.y<22) do
          begin
               delay(3000);
               drawpiece(False);
               inc (t);
               LastKey:=3;
               if (t=10) Then begin inc(piece.y);t:=0;end;
               If (KeyPressed) Then
               case ReadKey of
                    'q':NewPiece;
                    chr(27):
                    begin
                         textmode (C80);
                         clrscr;
                         Exit;
                    end;
                    ' ':begin inc(piece.rotate);piece.rotate:=piece.rotate mod 4;end;
                    chr(0):
                    case ReadKey of
                         chr(72):dec(t);
                         chr(75):
                         begin
                              LastKey:=0;
                              dec(Piece.x);
                         end;
                         chr(77):
                         begin
                              LastKey:=1;
                              inc(Piece.x);
                         end;
                         chr(80):if (t<>0) then inc(Piece.y);
                    end;
		 end;
		 drawpiece(True);
	   end;
     end;
end.