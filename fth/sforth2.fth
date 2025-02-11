START_LOADING_FILE

." Interactively loading SFORTH2.FTH\N"

( ============= SFORTH2:
Various testing / finagling code, new features / upgrades.
Placed in this file to iterate without rebuilding all of the
sforth1 code each time
- messing with ansi escape codes / rendering ( A_ funcs )
- messing with fixed-point arithmetic a little?
- messing with timing (CDELAY, RATECHECK, SBAR, BARTEST )
- messing with iteration/maps (RANGE, TABLE)
- compile tweaks (COMPILE_ONLY, CSP:!, CSP:? for depth-checks )
- trying a new text-input-buffer (TIB)
- new prompt/interpreter: (INTERPRET3:)
- grid stuff / GOL (DEMO)
- PROMPT2: shows current base

)

: STAR [CHAR] * EMIT ;
: STARS ( n ) 0 ?DO STAR ?LOOP ;

: U.03L ( f -- ; prints like 001 ) <# # # # #> ;
: .F ( f -- ) SIGN 1000 U/MOD SWAP ( frac int -- )
    0 U.R  [CHAR] . EMIT  U.03L ;

3142 CONSTANT PI

( ====== ANSI ART ====== )

: [EMIT] [POSTPONE] [CHAR]  [COMPILE] EMIT ; IMMEDIATE

27 CONSTANT A_ESC
: CSI A_ESC EMIT [EMIT] [ ;

CHAR 0 CONSTANT BLACK
CHAR 1 CONSTANT RED
CHAR 2 CONSTANT GREEN
CHAR 3 CONSTANT YELLOW
CHAR 4 CONSTANT BLUE
CHAR 5 CONSTANT MAGENTA
CHAR 6 CONSTANT CYAN
CHAR 7 CONSTANT WHITE
CHAR 9 CONSTANT NOCOL

: FG ( col -- ) [EMIT] 3 ( color ) EMIT ;
: BG ( col -- ) [EMIT] 4 ( color ) EMIT ;

: A; [EMIT] ; ;
: A_FMT [EMIT] m ;

: BEETEST CSI BLACK FG A; YELLOW BG A_FMT ;
: A_RESET CSI [EMIT] 0 [EMIT] m ;

: U.0 ( n ) BASE @ SWAP DECIMAL <# # #S #> BASE ! ;
: A_HOME CSI [EMIT] H ;
: A_XY  ( x y -- ) CSI U.0 A; U.0 [EMIT] H ;
: A_NUP    ( n -- ) CSI U.0 [EMIT] A ;
: A_NDOWN  ( n -- ) CSI U.0 [EMIT] B ;
: A_NRIGHT ( n -- ) CSI U.0 [EMIT] C ;
: A_NLEFT  ( n -- ) CSI U.0 [EMIT] D ;
: A_UP    ( n -- ) 1 A_NUP ;
: A_DOWN  ( n -- ) 1 A_NDOWN ;
: A_RIGHT ( n -- ) 1 A_NRIGHT ; 
: A_LEFT  ( n -- ) 1 A_NLEFT ;

: A_X   ( x -- ) CSI U.0 [EMIT] G ;
: A_START ( -- ) 0 A_X ;

( : A_SAVE A_ESC EMIT SPACE [EMIT] 7 ;
: A_REST A_ESC EMIT SPACE [EMIT] 8 ; )
: A_SAVE CSI [EMIT] s ;
: A_REST CSI [EMIT] u ;


: A_CLS_FWD  CSI [EMIT] 0 [EMIT] J ;
: A_CLS_BACK CSI [EMIT] 1 [EMIT] J ;
: A_CLS      CSI [EMIT] 2 [EMIT] J ;
: A_CLL_FWD  CSI [EMIT] 0 [EMIT] K ;
: A_CLL_BACK CSI [EMIT] 1 [EMIT] K ;
: A_CLL      CSI [EMIT] 2 [EMIT] K ;

: A_CURS_HIDE CSI ." ?25l" ;
: A_CURS_SHOW CSI ." ?25h" ;

: CLEAR A_CLS A_HOME ;

100 VARIABLE WIDTH
35 VARIABLE HEIGHT
: EMITDOWN EMIT A_DOWN A_LEFT ;
: TEST ( n char -- ) SWAP 0 ?DO DUP EMITDOWN ?LOOP DROP ;
: TEST ( n char -- ) DROP 0 ?DO I> # DROP DIGASCII EMITDOWN ?LOOP ;

: S A_SAVE ; : R A_REST ;
: TEST2 WIDTH @ 0 A_XY 
        HEIGHT @ [CHAR] X TEST ;
: TEST3 S TEST2 R ;

( ===== interactive stuff ==== )
: 5NOP NOP NOP NOP NOP NOP ;
( 38 CONSTANT DELAYCONST ( @38: 100 delay takes 1sec @ 1MHZ ) )
2 VARIABLE MHZ
: 10NOPLOOP ( n -- ) 0 DO 5NOP 5NOP LOOP ;
: CDELAY ( n -- ; 100ths of a sec @ 1mhz ) 0 DO 38 10NOPLOOP LOOP ;

( 38 10NOPLOOPs take 10ms)
( 30 10NOPLOOPs take 8ms)

( 30 CONSTANT DELAYCONST ( @47: 1 delay takes 8msec @ 1MHZ ))
( : MSLEEP ( n -- ) MHZ @ * ( 3 10NOPLOOPS take 1 MS ) 10NOPLOOP ; )

( 1000 DELAY takes about 13 sec @ 2MHZ with delayconst = 100 )
( 1000 DELAY takes about 5 sec @ 2MHZ with delayconst = 38, )

: RATECHECK 
    MHZ @ . ." MHZ: SHOULD TAKE 10 SECONDS" NL
    100 0 DO STAR 10 MHZ @ * CDELAY LOOP ;

: BAR ( n max -- ) [EMIT] [ OVER ( n ) STARS
    ( n max ) swap - SPACES
    [EMIT] ] ;

: SBAR ( n -- ; out of 20 ) S 0 A_X 20 BAR R ;
: BARTEST 21 0 DO I> SBAR 10 CDELAY LOOP ;

( ===== FANCY PROMPT ==== )
: INVERT ( -- ) CSI WHITE BG A; BLACK FG A_FMT ;
: STATPOS 0 A_X 99 A_NDOWN ;
: STATUS INVERT STATPOS WIDTH @ SPACES STATPOS .S A_RESET ;
: PROMPT2 PROMPT1 S STATUS R ;
( ' PROMPT2 'PROMPT ! )

3 VARIABLE STAT_LINES 
: LINES ( n ) BEGIN DUP 0> WHILE 1- NL WEND DROP ;
: CLEARSTATUS S A_CLS_FWD R ;
: MAKESPACE STAT_LINES @  DUP LINES A_NUP ;
: PROMPT3 CLEARSTATUS MAKESPACE PROMPT2 ;
( ' PROMPT3 'PROMPT ! )

( ===== TEST CURSOR READING? ==== )
: A_POS CSI [EMIT] 6 [EMIT] n ;
: READ_POS BEGIN KEY DUP .
            [CHAR] R = UNTIL ;
: TESTPOS HEX NL NL A_POS NL NL READ_POS NL NL ;

( ====== TABLE GEN ====== )

( new kind of variable: RANGE: { step start stop } )
: RANGE ( start stop step TOKEN -- )
    ( step ) [POSTPONE] VARIABLE
    ( start stop ) SWAP , , ;

: RG>LB ( p -- stop start ) DUP 2+ @ SWAP 1+ @ ;
: MAP ( xt range -- ) 
    ( xt: x -- )
    SWAP ( range xt )
    OVER RG>LB DO ( range xt )
        I> OVER EXECUTE
    OVER @ +LOOP 2DROP ;

0 VARIABLE 'TFUNC
: TABROW ( f -- ) 
    DUP .F ." -> " 
    'TFUNC @ EXECUTE .F
    NL ;

: TABLE ( xt range -- )
    SWAP 'TFUNC !
    ['] TABROW SWAP ( xt range ) MAP
    ;

: TEST1.5 ( f -- f ) 3 * 2 U/ ;
1000 2000 63 RANGE X

( ' TEST1.5 X TABLE )


( ==== NEW MATH STUFF ==== )

: 0! ( a -- ) 0 SWAP ! ;
: 1! ( a -- ) 1 SWAP ! ;

: MIN ( a b -- min ) 2DUP <= IF DROP ELSE NIP THEN ;
: MAX ( a b -- max ) 2DUP <= IF NIP  ELSE DROP THEN ;
: CLAMP  ( v start end -- v ) 1- ROT MIN MAX ;
: CLAMP= ( v min max -- v ) ROT MIN MAX ;

: 2@ ( p -- a b ) DUP @ SWAP 1+ @ ;
: 2! ( a b p -- ) SWAP OVER 1+ ( a p b p+1 ) ! ! ;

: 2OVER ( x y  a b -- x y  a b  x y ) 3 PICK 3 PICK ;

: ARRAY ( len )
    HERE TOKEN CREATE LATEST !  ( create/link header )
    DO_VAR ,
    ( len ) ALLOT ;

: PAIR ( a b ) 
    2 ARRAY  ( create pair )
    LATEST @ >CFA EXECUTE ( get its addr )
    ( a b addr ) 2!  ;

: 2. ( a b -- ) SWAP . . ;
: 2? ( addr -- ) 2@ 2. ;

( ==== NEW COMPILE STUFF ==== )

: MYNAME ( -- strp )
    WIP @ ?DUP IF ( if we're in a word )
        >WNA #, ( push our name )
    ELSE  STR" ??? " #, THEN
    ; IMMEDIATE

: _COMPILE_ONLY ( strp -- )
    STATE @ 0= IF ." ERROR: "
        ( name of parent ) TELL ."  called while not in compile mode\n"
        HANDLE_ERR 
    ELSE DROP THEN ;

: COMPILE_ONLY [POSTPONE] MYNAME ( name of parent ) [COMPILE]  _COMPILE_ONLY ; IMMEDIATE



: FINISHWIP ( -- ; if WIP word, link it in ) 
    WIP @ ?DUP IF LATEST ! THEN
    WIP 0! ;

( Used in compiling : and ;, so not immediate )
: CSP:! DEPTH CSP: ! ;
: CSP:? DEPTH CSP: @ <> IF CSP: @ CSP_FAIL THEN ;

: :NONAME ( -- HERE ) CSP:!
    HERE
    DO_COLON ,
    STATE 1!
    ;
: ; COMPILE_ONLY 
    CSP:? 
    STATE 0! 

    [COMPILE] EXIT FINISHWIP
; IMMEDIATE
 

( ==== TIB STUFF ==== )
: INC 1+! ;

128 CONSTANT #TIB
#TIB 1+ ARRAY _TIB
_TIB 1+ CONSTANT TIB ( points to start of tib, leaves room for strlen )

: PAD DP @ 64 + ;
: STRCPY ( src dest -- ) OVER @ 1+ -ROT ( len src dst ) MEMCPY ;

0 VARIABLE TIBSRC
0 VARIABLE TIBEND
: TIBWIPE ( -- ) TIB DUP TIBSRC ! TIBEND ! ;
: TIB, ( c -- ) TIBEND @ ! TIBEND INC ;

: TIBSHOW ( -- ) TIB #TIB DUMP ;

: READLINE ( -- ) TIBWIPE BEGIN
    KEY DUP TIB, '\N' = UNTIL ;


: ISBLANK ( c -- b ) DUP BL = SWAP '\N' = OR ;
    

: ADVANCE_WHILE ( endchar -- ; )
    TIBEND @ TIBSRC @ ?DO ?LOOP
    TIBSRC @ TIBEND @ >= IF DROP RETURN THEN 
    TIBSRC @ @ OVER <> IF DROP RETURN THEN
    TIBSRC INC TAILCALL RECURSE ;

: ADVANCE_TO_BLANK ( -- ) 
    TIBSRC @ TIBEND @ OVER ?DO ( srcp ) 
        DUP @ ISBLANK IF LEAVE THEN 
        1+
    ?LOOP ( srcp ) TIBSRC !  
    ;
: ADVANCE_UNTIL ( endchar -- ; advances srcp until next endchar )
    DUP BL = IF DROP ADVANCE_TO_BLANK RETURN THEN
    TIBSRC @ TIBEND @ >= IF DROP RETURN THEN 
    TIBSRC @ @ OVER =    IF DROP RETURN THEN
    TIBSRC INC TAILCALL RECURSE ;
: TRIM ( -- ; discards spaces from tibsrc ) BL ADVANCE_WHILE ;
: DISCARDLINE ( -- ) '\N' ADVANCE_UNTIL ;


: WRITELEN ( startp -- strp ; end is TIBSRC, writes len to startp-1, returns strp )
    TIBSRC @ ( start end ) OVER - ( start len ) SWAP 1- ( len strp ) TUCK ! ;

: ?COMMENT ( ) ;
: TIBTOKEN ( endchar -- strp )
    DUP BL = IF TRIM THEN
    TIBSRC @ @ [CHAR] \ = IF DISCARDLINE THEN
    ( check if starts with \, line comment )
    ( check if starts with \n, return as own token TODO: separate endchar? )
    TIBSRC @ SWAP ( startp endchar )
    ADVANCE_UNTIL ( startp )
    WRITELEN ( strp ) 
    ;
    
( TIBTOKEN: ( endchar -- strp )
    stashes strp
    collects chars until $endchar by advancing tibsrc
    calcs len, writes back to strp-1
    returns strp )
    BEGIN UNTIL
( QUESTION: how does readline signal long input? ( no \n in TIB )
            how does readline signal EOF?
    Q: If there's no token left, what to do?
        keep end-char out of bounds? 
        TOKEN with no more input should return a 0-length string? 
        READLINE with no more input should leave TIB empty, set a flag 
        TOKEN at EOF should return


        DESIGN OPTIONS:
        ?TOKEN returns err at EOF, TOKEN panics EOF 
        TOKEN returns empty string, INTERPRET checks for empty string token 


    TODO: need to handle the \n separateley ( if sep is BL, go until BL or '\n' )
    No point in leaving the \n in TIB, since, we don't do multiline tokens anyway?

    )
            

( TODO: if endchar == '\n' and tibsrc @ @ == '\n', return just the \n )

: TESTREAD KEY DROP READLINE ; ( drop the \n )
: TIBSHOW 
    ." Src/end: (" tibsrc @ tib - . ." , " tibend @ tib - . ." )\n" 
    TIBSHOW ;
          


: ?NUMBER ( n -- [n] ) STATE @ IF #, THEN ; ( leave on stack or compile in )
: ?PROMPT ( -- ; prompt if needed ) BULKLOAD @ 0= STATE @ 0= AND IF PROMPT THEN ;
: ?OK ( -- ; prints OK and/or PROMPT if needed  )
    BULKLOAD @ 0= STATE @ 0= AND IF
            SPACE ." OK" NL
            PROMPT
    THEN ;

: QUESTION ( tok -- ) TELL [EMIT] ? NL HANDLE_ERR ;
: INTERPRET3
    BULKLOAD @ 0= IF PROMPT THEN
    ( initial prompt ) 
    BEGIN
    ?PROMPT READLINE ( vector by mode? )


    ( LOOP : check \n: ok, prompt readline
             check EOF: )

    TOKEN UPPER CASE ( tok )
        DUP ?FIND ( tok (wha|0) ) ?DUP IF 
            ( tok wha ) NIP ?EXECUTE ?STACK ENDOF
        DUP NUMBER ( tok 0 | tok n 1 ) IF NIP ( n ) ?NUMBER ENDOF

        DUP 1+ @ '\N' = IF DROP ?OK ENDOF ( end of line, prompt if needed )
        DEFAULT ( unrecognized token ) QUESTION ENDOF 
    ENDCASE

    REPEAT
    ;
( ==== GRID STUFF ==== )


( temp overwrite grid )
20 width ! 20 HEIGHT ! 

WIDTH @ HEIGHT @ * ARRAY _GRID1
WIDTH @ HEIGHT @ * ARRAY _GRID2

_GRID1 VARIABLE CURRGRID
: GRID CURRGRID @ ;
: GRID1 _GRID1 CURRGRID ! ;
: GRID2 _GRID2 CURRGRID ! ;

( row-major )
: XY ( x y -- offset ) WIDTH @ * + ;
: >XY ( offset -- x y ) WIDTH @ U/MOD ( y x ) SWAP ;


: CLIPX ( u -- u ) 0 WIDTH @ CLAMP ;
: CLIPY ( u -- u ) 0 HEIGHT @ CLAMP ;
: 2CLIP ( x y -- x y ; if x y is out of bounds, wrap around )
    CLIPY SWAP CLIPX SWAP ; 
    
: PAIR+ ( a b  x y -- a+x b+y ) ROT + ( a x b+y ) -ROT + SWAP ;

: G! ( v x y -- ) XY GRID + ! ;
: GSET1 ( x y -- ) 1 -ROT G! ;
: GSET0 ( x y -- ) 0 -ROT G! ;
: G@ ( x y -- v ) XY GRID + @ ;

: OFFSET ( x y dx dy -- x y x' y' ) 2OVER PAIR+ ;
: SETOFFSET ( x y dx dy -- x y ) OFFSET 2CLIP GSET1 ;
: GETOFFSET ( x y dx dy -- x y v ) OFFSET 2CLIP G@ ;


0 VARIABLE #NEIGHBORS

( checkneighbor )
: CN ( x y dx dy -- x y ) debug GETOFFSET IF #NEIGHBORS 1+! THEN ;
: COUNTNEIGHBORS ( x y -- n ) 
    GRID1 #NEIGHBORS 0!
    -1 -1 CN  0 -1 CN 1 -1 CN
    -1  0 CN          1  0 CN
    -1  1 CN  0  1 CN 1  1 CN 
    ( x y ) #NEIGHBORS @ -ROT GRID2 G!
    ;


: UPLEFT ( p -- p ) WIDTH @ - 1- ;
: NXT ( p count -- p+1 count+? ) OVER @ + SWAP 1+ SWAP ;
: ROW ( p count -- p+row count ) SWAP WIDTH @ + 3 - SWAP ;
: UCN ( p -- count ) UPLEFT 0 ( p 0 )
    NXT NXT NXT ROW 
    NXT SWAP 1+ SWAP NXT ROW 
    NXT NXT NXT 
    ( p count ) NIP ;
: UCN2 ( x y -- ) 2DUP XY GRID1 GRID +  ( p ) UCN ( x y count ) 
        -ROT GRID2 G! ;
    

: ALLNEIGHBORS_OLD ( -- ) 
    HEIGHT @ 0 DO WIDTH @ 0 DO 
        I> J> debug COUNTNEIGHBORS
    LOOP LOOP ;

: ALLNEIGHBORS ( -- ) 
    HEIGHT @ 1- 1 DO WIDTH @ 1- 1 DO 
        I> J> debug UCN2
    LOOP LOOP ;

: LIFERULE ( state #neihbors -- b )
    SWAP  IF 2 3 WITHIN= ( if alive )
        ELSE   3       = THEN ;

0 0 PAIR V
: VFETCH ( -- u ) V 2@ G@ ;
: UPDATECELL ( x y -- )
    V 2!
    V 2@ GRID1 G@ ( state )
    V 2@ GRID2 G@ ( state #neighbors )
    LIFERULE ( newstate )
    V 2@ GRID1 G! ( writeback )
    ;

: D TAILCALL DCONT ; ( for debugging )


: ALLUPDATE ( -- ) HEIGHT @ 0 DO WIDTH @ 0 DO I> J> UPDATECELL LOOP LOOP ;
: TICK ( -- ) ALLNEIGHBORS ALLUPDATE ;



0 VARIABLE 'SHOWCELL
: SC_# ( v -- [emits] ) IF [EMIT] # ELSE [EMIT] . THEN ; 
: SC_DIG ( v -- [emits] ) 
    CASE
        DUP 0< IF DROP [EMIT] n ENDOF
        DUP 35 < IF DIGASCII EMIT ENDOF
        DEFAULT [EMIT] ? ENDOF
    ENDCASE ;

' SC_# 'SHOWCELL !

: GSHOW ( x y -- ) G@ 'SHOWCELL @ EXECUTE ;

: DRAWF ( x y -- )
                   0 -1 SETOFFSET 1 -1 SETOFFSET 
    -1 0 SETOFFSET 0  0 SETOFFSET
                   0  1 SETOFFSET
    2DROP ;

: DRAWBAR ( x y -- ) -1 0 SETOFFSET 0 0 SETOFFSET 1 0 SETOFFSET 2DROP  ;
: DRAWGLIDE ( x y -- )
                   0 -1 SETOFFSET                
                                  1  0 SETOFFSET 
    -1 1 SETOFFSET 0  1 SETOFFSET 1  1 SETOFFSET
    2DROP ;


: GCLEAR ( -- ) GRID1 HEIGHT @ 0 DO WIDTH @ 0 DO I> J> GSET0 LOOP LOOP ;
: GINIT ( -- ) gclear 10 10 DRAWF ;
: GINITBAR ( -- ) GCLEAR 10 10 DRAWBAR ;

: GDUMP ( -- ) 
    HEIGHT @ 0 DO 
        WIDTH @ 0 DO
            I> J> GSHOW 
        LOOP NL
    LOOP ;

: GDRAW ( -- ) S A_CURS_HIDE 
    A_HOME 3 LINES ( A_CLS_BACK ) 
    GDUMP 
    A_CURS_SHOW R ;
: RUN1 tick STAR gdraw ;
: RUN 10 0 do RUN1 loop ;


: DEMO NODEBUG CLEAR GINIT GCLEAR 5 10 DRAWGLIDE RUN ;
( test
GINIT GCLEAR 5 10 DRAWGLIDE GDRAW
NODEBUG
)

: SHOWBASE ( emits a char )
    BASE @ CASE
        2 OF [EMIT] b ENDOF
        10 OF [EMIT] d ENDOF
        16 OF [EMIT] x ENDOF
        DEFAULT DIGASCII EMIT ENDOF 
    ENDCASE ;
: PROMPT2 SHOWBASE SPACE DEPTH 0 .R [EMIT] > SPACE ;
' PROMPT2 'PROMPT !


END_OF_FILE
