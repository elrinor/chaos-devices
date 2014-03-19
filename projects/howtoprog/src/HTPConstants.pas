unit HTPConstants;

interface
const MAX_STRING_NUMBER=200;
      MAX_STRING_LENGTH=200;
      MAX_COMMENT_LENGTH=78;
      SCREEN_HEIGHT=19;
      SCREEN_WIDTH=78;
      CEmptyString78 = '                                                                              ';

      clWhitespace=          0;
      clComment=             1;
      clString=              2;
      clIdentifier=          3;
      clNumber=              4;
      clReservedWord=        5;
      clSymbol=              6;
      clCodeWindowBack=      7;
      clCodeWindowBorder=    8;
      clCodeWindowHeader=    9;
      clCodeWindowScroller= 10;
      clCodeWindowSprainer= 11;
      clCodeWindowButtons=  12;
      clCommentWindowBack=  13;
      clCommentWindowBorder=14;
      clCommentWindowHeader=15;
      clCommentColor       =16;

      ReservedWords:array[1..48]of String[15]=('ABSOLUTE','AND','ARRAY','BEGIN','CASE','CONST','DIV','DO','DOWNTO','ELSE','END','EXTERNAL','FILE','FOR','FORWARD','FUNCTION','GOTO','IF','IMPLEMENTATION','IN','INLINE','INTERFACE','INTERRUPT','LABEL','MOD','NIL','NOT','OF','OR','PACKED','PROCEDURE','PROGRAM','RECORD','REPEAT','SET','SHL','SHR','STRING','THEN','TO','TYPE','UNIT','UNTIL','USES','VAR','WHILE','WITH','XOR');
      IdentifierCharacters:set of Char=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','_','1','2','3','4','5','6','7','8','9','0'];
      IdentifierStartCharacters:set of Char=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','_'];
      NumberCharacters:set of Char=['0','1','2','3','4','5','6','7','8','9'];
      NonSymbolCharacters:set of Char=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','_','1','2','3','4','5','6','7','8','9','0','''','{'];

implementation

end.
