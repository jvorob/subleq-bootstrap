SILENT
: IMMEDIATE 1 LATEST @ 1+ ! ;
: [ 0 STATE ! ; IMMEDIATE
: ] 1 STATE ! ;
: CHAR TOKEN 1+ @ ;
: C' LW_LIT , CHAR , ; IMMEDIATE
: (  KEY C' ) =
    [ LW_0BRANCH , -5 , ] ; IMMEDIATE
( We have comments now! not nested though )

( Use this if we need to break to EOF before the v2 interpreter is up)
: SKIPTO$ C' S EMIT
    KEY C' $ =
    [ LW_0BRANCH , -5 , ] ;

( ======== TODOS:
- Finish ?STACK
    - Check stack underflow, rstack underflow
    - Check canaries
    - Error out? [reset data stack]
- More error handling
    - COMPILE_ONLY flag?
- write DUMP
    - probably needs U.R

 Less urgent
 - Add forth version constants?
 - fix unsigned . [e.g. FFFF U.]
 - VALUE, TO VALUE?
 - fancy loops [access to I?]

 Long-term goals:
 - CASE?
 - VALUE X, TO X, +TO X
 - <CFA
 - SEE
)

( ============= MISC ============= )


: SP@ ( a -- a ptr_to_a ) NOSP_ @ NEG ;
: STACKCOUNT ( - num_items_on_stack)
    SP@  ( call this first to get clean SP val )
    DS0 SWAP  ( stack_base, nosp )
    - ( base-sp )
    1+  ( +1, to include the value at SP  )
    ;


: 'A' C' A ;
: 'Z' C' Z ;
: '0' C' 0 ;
: '(' C' ( ;
: ')' C' ) ;
: '"' C' " ;
: '\N' 10 ;
: '\\' 92 ;


( Stack helpers )
: NIP ( a b - b ) SWAP DROP ;

( currently these seem to work for small positive divisors)
: U/ ( a b - a/b )
    U/MOD ( a/b a%b ) DROP ;
: UMOD ( a b - a%b )
    U/MOD ( a/b a%b ) NIP ;

: HEX 16 BASE ! ;
: DECIMAL 10 BASE ! ;

( : @! @ ! ; )
( UHHHH THIS WORKS THE OPPOSITE OF HOW I'D WANT IT TO.
  Disable this for now .
  I want SRC @ TGT !, but this does TGT SRC @ ! )

( ============== SCRIBING WORDS =========== )

( In future we'll add error-handling to FIND so it can error out
 if not found. However we still want access to the base FIND
 for cases where we handle the returns-0 )
: ?FIND ( strp - [cfa or 0]) FIND ;
: FIND ( strp - cfa [ errors if not found]) FIND ;
( all subsequent uses of find will use this, which we'll patch to do
  errorchecking )

: ALLOT DP +! ;
: #, ( n -- ; compiles TOS as a literal number ) LW_LIT , , ;
: ' ( - CFA ; lookup CFA of next token ) TOKEN FIND >CFA ;
( NOTE: later we update ' so it quotes at compile time as well )

: >WNA ( WHA -- strp ) 2+ ;  ( gives str pointer to a word's name )

( we'll use this to make sure we don't accidentally forget everything )
: DICT_START [ TOKEN IMMEDIATE FIND #, ] ;
( points to start of non-core dict )

: FORGET' ( -- ) ( Looks up next token, resets LATEST and dictionary to before it )
    TOKEN FIND ( wha )
    DUP DP ! ( wipe out dictionary after wha  )
    @ LATEST ! ( get previous wha, set as latest )
    ;

: PATCH ( cfa_old cfa_new -- )
    ( overwrites old word to be : {OLD} TAILCALL {NEW} )
    SWAP ( new old )
    DO_COLON OVER ! ( new old ; makes old into a secondary )
    1+ LW_TAILCALL OVER ! ( new old+1 ; writes old[1]=tailcall )
    1+ ( new old+2 )
    ! ; ( -- ; writes old[2] = new )


( ============== COMPILE WORDS =========== )

( Define postpone, useful for making compiling words that
  use other compiling words at macroexpand time
  e.g. : WHILE [POSTPONE] IF ;
)
: [POSTPONE] ( <reads immediate word> -- <compiles it for later> )
    ' , ; IMMEDIATE

: RECURSE ( -- [compiles word being currently defined] )
    WIP @ >CFA , ; IMMEDIATE

( TAILCALL a word, i.e. the next word will not return to us, it will
    return to our caller )
: TAILCALL LW_TAILCALL , ; IMMEDIATE

( should function identically to exit, but has a different CFA,
  should help in disassembly )
: RETURN TAILCALL ; ( will tailcall its own exit )

( ============= VARIABLES ============= )

: VARIABLE ( init_val [TOKEN] - )
    ( creates a new word that returns ptr to value)
    DP @         ( init_val hdr -- store current addr )
    TOKEN CREATE ( create header, links into latest )
    LATEST !     ( init_val -- : link word into latest)
    DO_VAR ,     ( set codefield as DO_VAR )
    ,            ( enclose initial value )
;

: CONSTANT ( init_val [TOKEN] - )
    ( creates a new word that returns value)
    DP @         ( init_val hdr -- store current addr )
    TOKEN CREATE ( create header, links into latest )
    LATEST !     ( init_val -- : link word into latest)
    DO_CONST ,   ( set codefield )
    ,            ( enclose initial value )
;


( ========== CONTROL FLOW ========= )

( BEGIN loop test UNTIL )
( BEGIN loop REPEAT )
: BEGIN ( -- begin_addr )
    DP @  ( push HERE to rstack )
    ; IMMEDIATE

: UNTIL ( begin_addr -- ; compiles 0BRANCH, offset )
    LW_0BRANCH , ( enclose 0branch)
              ( begin_addr  -- )
    DP @ -    ( calculate offset:  begin_addr-HERE )
    ,  ( enclose offset, tgt-curr )
    ; IMMEDIATE

: REPEAT ( begin_addr -- ; compiles BRANCH, offset )
    LW_BRANCH , ( enclose 0branch)
              ( begin_addr  -- )
    DP @ -    ( calculate offset:  begin_addr-HERE )
    ,  ( enclose offset, tgt-curr )
    ; IMMEDIATE



( test IF a ELSE b THEN )
( compiles to:
  test 0BRANCH to_else a BRANCH to_then offset b _
)
: IF ( -- if_fixup )
    LW_0BRANCH , ( enclose 0BRANCH )
    DP @         ( push addr of offset )
    1 ALLOT      ( leave room for offset)
    ; IMMEDIATE

( compiles a BRANCH to end of THEN )
( sets if_fixup to point into else case )
( pushes else_fixup )
: ELSE ( if_fixup -- else_fixup )
    LW_BRANCH , ( enclose BRANCH )
    DP @  ( if_fixup else_fixup )
    SWAP  ( e_fix i_fix)

    1 ALLOT ( ;leave room for offset )

    ( calculate offset that the if's 0branch should jump to
      Needs to jump into the else case, which is where we are now
      do here - if_fixup )

    DP @    ( e_fix i_fix here )
    OVER -  ( e_fix i_fix  here-i_fix )
    SWAP !  ( e_fix  ; writes offset into 0BRANCH from if )
    ; IMMEDIATE

: THEN ( if_fix -- ; note: can takes either if_fix or else_fix )
    ( calculate offset: HERE - if_fix )
    DP @    ( if_fix here )
    OVER -  ( if_fix here-if_fix )
    SWAP !  ( ; write offset into BRANCH/0BRANCH from before )
    ; IMMEDIATE


( BEGIN test WHILE loop WEND )
( compiles to:
  _ test 0BRANCH to_wend loop BRANCH to_test
)
( NOTE: while compiles exactly like if, but need to escape it)
: WHILE [POSTPONE] IF ; IMMEDIATE

( compiles a branch back to BEGIN,
  fixes up WHILE's offset to point after itself )
: WEND ( begin_addr while_fix_addr -- )
    LW_BRANCH ,
    ( calculate offset to begin: begin-HERE)
    SWAP ( while_fix begin_addr )
    DP @ - , ( enclose offset to begin )

    ( calculate offset for while_fix to come here )
    ( while_fix -- )
    DP @ ( while_fix here -- )
    OVER - ( while_fix HERE-while_fix -- )
    SWAP ! ( -- ; set while_fix offset to come here)
    ; IMMEDIATE


( ========== NESTED COMMENTS ========= )

: (
    1 ( nesting_depth )
    BEGIN DUP 0> WHILE ( loop until paren depth 0 )
        KEY ( depth char -- )
        DUP '(' = IF ( depth char )
            DROP 1+ ( depth+1 )
        ELSE
            DUP ')' = IF ( depth char )
                DROP 1- ( depth-1)
            ELSE
                DROP
            THEN
        THEN
        ( depth )
    WEND
    DROP ( )
    ; IMMEDIATE

( ( WOO HOO WE CAN DO NESTED ) )

( ============ DO/LOOP ============ )

: DO ( end start -- RS: end start )
    LW_DO , ( enclose lw_do )
    DP @ ( stash loopback label )
    ; IMMEDIATE

( for all 3: stack is ( loopstart -- ) )
: LOOP 1 #, LW_+LOOP ,
    DP @ - , ( offset to label: label-here )
    ; IMMEDIATE
: +LOOP LW_+LOOP ,
    DP @ - , ( offset to label: label-here )
    ; IMMEDIATE
: -LOOP LW_-LOOP ,
    DP @ - , ( offset to label: label-here )
    ; IMMEDIATE

: ['] ' #, ; IMMEDIATE
( ?DO ?LOOP compiles as `2DUP < IF DO .. LOOP ELSE 2DROP THEN` )
: ?DO ( -- marker-IF, marker-DO )
    ['] 2DUP , ['] > , [POSTPONE] IF
    [POSTPONE] DO
    ; IMMEDIATE
: ?+LOOP ( marker-IF marker-DO -- )
    [POSTPONE] +LOOP
    [POSTPONE] ELSE
        ['] 2DROP , [POSTPONE] THEN
    ; IMMEDIATE
: ?LOOP ( marker-IF marker-DO -- )
    1 #, [POSTPONE] ?+LOOP
    ; IMMEDIATE

: FLIP?DO ( marker-IF marker-DO -- markerIF marker-DO )
    OVER 2 - ( addr of > )
    ['] < SWAP ! ( overwrite )
    ;
: ?-LOOP ( marker-IF marker-DO -- )
    FLIP?DO ( switch test to use < instead of > )
    [POSTPONE] -LOOP
    [POSTPONE] ELSE
        ['] 2DROP , [POSTPONE] THEN
    ; IMMEDIATE


( : I> R@ ; )
: J> ( RS: jend j iend i ra -- ) 3 RPICK ;
: K> 5 RPICK ;

: LEAVE ( sets I = end )
    ( RS: end I ra ) R>
    R> DROP ( RS: end )
    R@ >R >R ( RS: end end ra ) ;

( === loop helpers === )
: STRBOUNDS ( strp -- &str[0] &str_end )
    DUP @ ( strp len -- )
    OVER + 1+ ( strp strend )
    SWAP 1+ ( strend str[0] )
    ;

( ========== OUTPUT . and friends ========= )



: SPACES ( n -- [ outputs n spaces, min 0 ] )
    BEGIN DUP 0> WHILE SPACE 1-
    WEND ( n )
    DROP ;

: ABS DUP 0< IF NEG THEN ;

: DIGASCII ( dig -- [outputs ascii] )
    DUP 36 >= IF ( n )
        DROP C' ? RETURN
    THEN DUP 10 >= IF ( n )
        10 - 'A' + RETURN ( digit in 10-35, A-Z )
    THEN DUP 0 >= IF ( n )
        '0' + RETURN ( digit in 0-9 )
    THEN  ( n )
        DROP C' ?
    ;


: CSTACKSHOW C' : EMIT STACKCOUNT DIGASCII EMIT SPACE  ;
: U.R  ( number width -- ; prints out unsigned,
    rpadded to width )
    ( we're going to push the digits onto the stack, LSD on bottom )

    SWAP ( width is going to be under all this mess )


    -1 SWAP  ( push -1 as sentinel )
    BEGIN ( width -1 [..digits] rest )
        BASE @ U/MOD ( -1 [digits] rest digit )
        ( In the absence of an unsigned divmod, manually unsign the digits? )
        ( e.g. in hex, remainder of -1 is )
        DUP 0< IF BASE @ + THEN


        ABS ( TODO: BUG we need an unsigned divmod, but for now we make do )
        SWAP        ( -1 [digits] digit rest )
    DUP 0= UNTIL ( if rest = 0, break )

    ( width -1 [digits] 0 )
    ( leave 0 on stack, we'll use it as num chars printed )

    ( we're guaranteed at least one digit)
    BEGIN ( width -1 [digits] cnt )
        1+ SWAP ( ... [digits] cnt nextdigit )
        ABS DIGASCII EMIT
    ( width -1 [digits] cnt )
    OVER 0< UNTIL ( stop when we hit the -1 )
    NIP  ( width cnt ; drop the -1 )

    ( Now, let's pad: prints width-count spaces )
    - SPACES
    ;

: .R ( number width -- prints signed, padded to width )
    OVER 0< IF ( if number negative )
        C' - EMIT ( print '-' )
        1-       ( decrement width (we have '-') )
        SWAP NEG ( negate number )
        SWAP ( number width )
    THEN U.R ;

: U. ( number -- prints out unsigned, then a space )
    0 U.R ( prints unpadded )
    SPACE ; ( add a space )


: . ( number )
    DUP 0< IF
        C' - EMIT ( print '-' )
        NEG  ( negate number )
    THEN U. ;

: ? ( addr -- ) @ . ;

DECIMAL
: TOP4EMIT ( n -- [ prints top nibble] ) 12 U>> DIGASCII EMIT ;
: XU.4 ( n -- [prints n properly, in hex] )
    DUP TOP4EMIT 4 <<
    DUP TOP4EMIT 4 <<
    DUP TOP4EMIT 4 <<
    TOP4EMIT ;
: X. XU.4 SPACE ;


( ==========  STACK SHENANIGANS  ========= )

( ACESSING NEAR TOS: )
( if tos contains a pointer to its own address, reading it is dicey.
  It tends to work in practice because of cache invalidation,
  But in theory a sequence like:
     ( x:42 -- )
     FETCH_PTR   ( x:42 tos:ptr_to_x ; ptr_t_x TOS )
     NIP  ( tos:ptr_to_x  ; ptr was never written into x)
     @    ( 42 ; stale data )
  Could cause issues. Since ptr_to_x never had to be written into
  cell x, it would read old data. NOTE: this would only happen if
  NIP was a primary

)

( Ends up pointing to TOS? but can't access TOS directly
  the actual TOS is kept in a register, so might not work correctly
  In practice however, even doing SP@ puts an extra item on the stack
  so it can mostly be accessed normally
)


: .S ( prints all items on the stack )
    STACKCOUNT . C' : EMIT SPACE
    ( we want to print ds0 first, so -LOOP. Include SP@ )
    SP@ 1- DS0  ( DS0 downto and incl SP@ )
    2DUP >= IF 2DROP RETURN THEN
    DO I> @ .  1 -LOOP
    NL ;


( ========== Fancier quoting/inlining ========= )


( Experimental? )
( TWO SCHOOLS OF THOUGHT
    - Textualist: `[A]` is the same as `A`, just marks it as IMMEDIATE
        e.g.  : [A] ( -- a ) A ; IMMEDIATE
    - Precompile: execute NOW, but output for use at macroexpand time
        e.g. : [A] ( -- ) A #, ; IMMEDIATE

        wrinkle: are we using [foo] in a normal word?
                  or is [foo] itself being used in a meta-word
        e.g.
            : A1 [CHAR] A #, ; IMMEDIATE
            : A2 [CHAR] A ;
        A1 is a compile word that compiles a literal A, [CHAR] is used textually
        A2 is a normal word that pushes A, [CHAR] is used to precompile
)

( ========== MISC ========= )
( - STATUS?:show DP, LATEST, SP RSP, etc
  - nested comments
)
DECIMAL
: ISPRINTABLE ( c -- 1/0 )
    ( true if in range 32-126 )
    DUP  31 >
    SWAP 126 <= AND ;

( update C' to work in NORMAL mode as well )
: C' CHAR
    STATE @ IF #, THEN ; IMMEDIATE

: ISASCIILOWER ( c -- bool ) DUP C' a >= SWAP C' z <= AND ;
: ASCIIUPPER DUP ISASCIILOWER IF [ 'A' C' a - #, ] + THEN ;
: UPPER ( strp -- strp [ uppercases the string] )
    DUP STRBOUNDS ?DO I> @ ASCIIUPPER I> ! ?LOOP ;

( update quote ' to be flexible, working in normal mode or compile mode
  also looks up by uppercasing )
: ' ( <finds next word> -- <pushes / compiles CFA > )
    TOKEN UPPER FIND >CFA ( lookup using old func )
    STATE @ IF #, THEN
    ; IMMEDIATE


( TODO
: MEMWRITE" ( pointer -- [reads string into memory at specified location] )
)

: STR", ( -- strp ; Reads a string using KEY, encloses in dict, stops at " )
    DP @    ( strp ; save a ptr to start of the string )
    1 ALLOT ( ; leave room for the length field )
    0       ( strp len )
    KEY DROP ( Drop the initial space )
    KEY ( ; get first char )

    ( strp len char )
    BEGIN DUP '"' <> WHILE ( loop until we hit a " )

        ( == check for escape sequences )
        DUP '\\' = IF ( strp len '\' )
            ( got \, fetch next char and check escapes )
            DROP KEY
            DUP ASCIIUPPER C' N = IF DROP '\N'  ( strp len '\N' )
            ELSE
                ( strp len escchar )
                ( just leave the escaped char to be enclose )
            THEN
        THEN ( strp len nextchar )
        ,  ( strp len ; enclose char)
        1+ ( strp len+1 )
        KEY ( get next key )
    WEND  ( strp len char )

    DROP  ( strp len )
    ( now, write length to str base )
    OVER !  ( strp )
    ; ( return strp )

: TEMPSTR" ( -- strp ; quotes a string, write into dict, but doesn't enclose )
    ( useful at command line )
    STR", ( strp ; read into dict )
    DUP DP ! ( reset DP to not include string )
    ; ( -- strp )

: STR" ( -- ; quotes a string, encloses in word as a literal )
    ( IMMED_ONLY )
    LW_LITSTR ,
    STR",  ( enclose string in dict )
    DROP ( STR", returns a string pointer??? TODO: RETHINK THIS)
    ; IMMEDIATE

: ." ( [reads string] -- ; tells it (VERSATILE) )
    STATE @ IF
        [POSTPONE] STR"
        ' TELL ,
    ELSE
        TEMPSTR" TELL
    THEN ; IMMEDIATE


( Patch CREATE to always uppercase its tokens )
: _NEW_CREATE ( str -- )
    ( same as original create just uppercase the tokens first)
    LATEST @ , ( strp -- ;link to prev word )
    0 ,        ( enclose flags )
    UPPER STR, ( uppercase token and enclose in dict )
    ;
 ' CREATE ' _NEW_CREATE PATCH

( ========== NEW OUTER INTERP, restart ========= )

: ?EXECUTE ( wha -- ; compiles or executes, based on STATE and IMMED?)
    STATE @ IF ( we're in compile mode )
        ( it's immediate, execute it )
        DUP IMMED? IF
             >CFA EXECUTE
        ELSE >CFA , ( else compile )
        THEN
    ELSE ( normal mode )
        >CFA EXECUTE
    THEN ;


: ?DUP  ( n -- [ n n OR 0 ] )
    ( duplicates if nonzero, useful for ?DUP IF )
    DUP 0> IF DUP THEN ;

( We wan't to behave differently when loading a forth file vs when in interactive mode
 In interactive mode, errors are reported immediately, and the rest of the line is skipped
 In reading_file mode, errors should break interpreting until the end of the file
)

0 VARIABLE INTERPETER  ( RESTART jumps to current value of this )
0 VARIABLE BULKLOAD ( Set to 0 by RESTART, set to 1 for bulk-loading,
                      disables OK and PROMPT on newline )

( === FILE LOADING MODE: === )

: START_LOADING_FILE ( switches into bulkload mode )
    NL STR" Loading until END_OF_FILE\n" TELL
    1 BULKLOAD ! ;

: END_OF_FILE ( switches out of bulkload mode)
    STR" Finished Loading" TELL NL
    0 BULKLOAD ! ;

: SKIP_TO_EOF ( -- num_lines_skipped)
    ( Used in bulkload mode to skip the rest of file input
      in case we encountered an error )
    ( TODO: make it process comments? )

    0 BEGIN ( num_skipped )
        TOKEN ( num_skipped token -- )
        DUP 1+ @ '\N' = IF ( if is \n )
            SWAP 1+ SWAP
        THEN
        STR" END_OF_FILE" STR= ( if is EOF, break )
    UNTIL
    ( -- num_skipped ) ;

: SKIP_TO_NL ( discards rest of input line until it hits a \n )
    BEGIN KEY '\N' = UNTIL ;


( Interpreter v2: key features:
 - Better error handling
   - Interactive:
     - if word not found: print WORD?, discard rest of line, return to prompt
     - if word not found in compile mode: same? leave compile mode? clear WIP?
     - if stack error: UNDERFLOW, discard line, reset stack, leave compile mode
     - if EOF: print "hit EOF", halt
   - Reading File:
     - if any error: print error
     - if EOF: print "hit EOF", halt
     - handle_error ( discard rest of file )
     - ( restart )
 - Prompt:
    - if interactive: after each newline, if not in compile mode, print OK NL PROMPT
)

: PROMPT  ( prints out "[n] > " )
    C' [ EMIT
    BASE @ DECIMAL  ( switch to decimal, save base )
    STACKCOUNT 1- .  ( subtract 1 to account for saved BASE )

    C' > EMIT SPACE
    BASE ! ( restore base )
    ;

: RESTART ( ??? )
    BASE @ 0= IF ( if starting first time )
        STR" Interpreter v0.2: starting" TELL NL
    ELSE ( restarting )
        STR" RESTART (v0.2)" TELL NL
    THEN

    ( TODO: reset BASE )
    ( TODO reset data stack )
    RS0 1+ NEG RSP_ ! ( reset return stack )

    DS0 2+ NEG NOSP_ ! ( reset data stack ??? )

    ( reset variables )
    0 BULKLOAD ! ( ; interactive )
    10 BASE !    ( ; decimal )

    ( INTERPETER @ EXECUTE )
    INTERPRET ( calls old INTERPRET, but we'll patch over it )

    STR" ERR: Returned from interpreter" TELL
    ;

: HANDLE_ERR
    ( we had a stack underflow, undefined word, etc.
      - discard rest of input (rest of line, or rest of file if BULKLOAD)
      clear any words mid-compilation, then restart )
    DECIMAL ( return to known state )

    STATE @ IF    ( if in compile mode, clean up WIP word )
        WIP @ DICT_START > IF ( double check we're not wiping out the whole dict )
            WIP @ DP !   ( reset DP to start of WIP word )
    THEN THEN
    0 WIP !     ( discard WIP word, if any )

    0 STATE ! ( switch out of compile mode )

    ( If we were bulk-loading a file, errors should-early exit the whole thing)
    BULKLOAD @ IF
        STR" ERROR LOADING FILE\n" TELL
        STR" - Last word was: " TELL
            LATEST @ >WNA TELL NL ( print last word )
        STR" - Skipped rest of file (" TELL
            SKIP_TO_EOF
            . STR" lines)\n" TELL
        0 BULKLOAD ! ( switch back to interactive mode )
    ELSE SKIP_TO_NL

    THEN

    RESTART
    ;

: ?STACK ( checks stacks for underflow, errors out if found )
    STACKCOUNT 0< IF
        STR" DS UNDERFLOW: " TELL
            STACKCOUNT .
            STR" items on stack" TELL NL
        HANDLE_ERR
    THEN ;

: NEW_INTERPRET
    BULKLOAD @ 0= IF PROMPT THEN

    BEGIN
    TOKEN UPPER

    DUP ?FIND ( tok wha|0 ; we use the checked find here, since we handle the 0)
    ?DUP IF ( ; found word )
        ( tok wha )
        SWAP DROP ( wha )

        ( if we're in compile mode and word is not immediate, compile it )
        ?EXECUTE

        ?STACK ( check stack for error. If found, discard input and error out )

    ELSE  ( tok )
        DUP NUMBER ( tok 0 | tok n 1 )
        IF ( tok n )
            NIP ( n )
            STATE @ IF #,
            THEN ( if normal mode, leave n on top of stack )

        ELSE ( either \n or invalid token )
            ( tok -- )
            DUP 1+ @ '\N' = IF
                ( tok -- ; Got newline, all is well )
                ( ; prompt, continue )
                DROP
                BULKLOAD @ 0= IF
                    STATE @ 0= IF
                        SPACE STR" OK" TELL NL
                        PROMPT
                    THEN
                THEN

            ELSE ( token failed to parse: error message, restart )
                TELL C' ? EMIT NL
                HANDLE_ERR
            THEN

        THEN THEN

    ( TODO: check stack over/underflow, raise errors, reset )

    REPEAT
    ;


( Now we have a better interpreter, let's patch the old interpreter )
( NOTE: this needs to happen all at once, so wrap in a temp upgrade word )
: UPGRADE
    ( Patch in new interpreter, switch to bulk load mode )
    STR" Upgrading interpreter to v0.2\n" TELL

    ( ' NEW_INTERPRET INTERPETER ! ( patch in interp ) )
    ( Overwrite interpret with our new one )
    ' INTERPRET ' NEW_INTERPRET PATCH

    0 BASE ! ( clear base so we hit the START logic )
    RESTART  ( restart into new interp, switch to load mode )
    ;
( Upgrade to the new interpreter, then immediately switch to BULKLOAD
  mode so we minimize unnecessary prompts )
UPGRADE START_LOADING_FILE
FORGET' UPGRADE  ( we don't actually want to keep upgrade )

( ================== NEW INTERPRETER EXISTS! ====================== )
( fix up old funcs to take advantage of error handling )

: STR" ( redefine, if not in compile mode, error out )
    STATE @ 0= IF
        STR" ERROR: STR\" only valid in compile mode\n" TELL
        HANDLE_ERR ( error, restart )
    THEN
    [POSTPONE] STR" ( call into old STR" )
    ; IMMEDIATE

( we can finally make FIND and friends take advantage of error checking )
: _CHECKED_FIND ( strp - cfa [ errors if not found ] )
    DUP ?FIND ?DUP IF
        ( strp cfa )
        NIP RETURN ( -- cfa )
    THEN ( if not found: )
    ( strp )
    STR" ERROR IN FIND: " TELL
        TELL  ( print offending token )
        STR" ?\n" TELL
    HANDLE_ERR
    ;

( patch FIND to use _CHECKED_FIND )
' FIND ' _CHECKED_FIND PATCH

( Upgrade : to error out if we call it in compile mode )
: : ( does normal : things )
    STATE @ IF
        STR" ERROR: : called while already in compile mode\n" TELL
        HANDLE_ERR ( restart )
    THEN [ ' : , ] ( call into old colon )
    ; IMMEDIATE

( ================== DEBUGGER  ====================== )


1 VARIABLE DEBUG_STATE
: YESDEBUG 1 DEBUG_STATE ! ;
: NODEBUG 0 DEBUG_STATE ! ;

( Exits two levels up, so calling from interpreter will return to debug )
: DCONT
    ( return addresses should be DEBUG INTERPRET ?EXECUTE )
    ( drop first 2, so this returns straight up to debug )
    R> R> 2DROP
    ;
: DEBUG
    DEBUG_STATE @ 0= IF RETURN THEN ( skip debugging if disabled )

    STR" \n === ENTERING DEBUGGER ===\n" TELL
    .S
    NEW_INTERPRET
    STR" \n === LEAVING DEBUGGER ===\n" TELL
    ;
: [DEBUG] TAILCALL DEBUG ; IMMEDIATE


( ==========  Arithmetic Test cases  ========= )
: TRUE 1 ;
: FALSE 0 ;
: EXPECT" ( a expected [ reads "errstr" from input ] -- )
    ( if a and b differs, prints error string, halts )
    TEMPSTR" -ROT ( strp a b )
    2DUP <> IF ( strp a b; if values differ from expected )
        ( f"ERROR ({errstr}), EXPECTED {a], GOT {b}" )
        STR" ERROR (" TELL
        ROT TELL ( ; print strp )
        STR" ): EXPECTED " TELL
        ( a exp )
        .  ( a; print exp)
        STR" , GOT " TELL
        . ( ; print a )
        NL

        HANDLE_ERR
    ELSE
        2DROP DROP ( -- )
    THEN ;

: TF ( 1/0 -- [emits T or F] ) IF C' T EMIT ELSE C' F EMIT THEN ;
: BNOT ( a -- !a ) 1 XOR ; ( negate a boolean )
: U> SWAP U< ;
: U>= U< BNOT ;
: U<= SWAP U< BNOT ;

( ==========  Arithmetic Test cases  ========= )
: TESTCASE_START ; ( used to forget all temp testing functions )


: U<SHOW ( a b -- )
    ( tries all permutations of comparison using U< )
    STR" UNSIGNED: a<b b<a a<a b<b: " TELL
    ( a b )
    2DUP U< TF
    2DUP SWAP U< TF
    ( a b ) OVER DUP U< TF
    ( a b ) DUP U< TF ( a )
    DROP ;

: TESTFAIL ( a b -- [prints failing case, errs out] )
    STR" TEST FAIL: (" TELL
        2DUP SWAP X. X. STR" -- ): " TELL
    ( a b ) U<SHOW NL
    HANDLE_ERR ;

: TEST< ( a b -- [ tests all signed comparisons ] )
    2DUP <  0= IF 1 . TESTFAIL THEN
    2DUP >     IF 2 . TESTFAIL THEN
    2DUP <= 0= IF 3 . TESTFAIL THEN
    2DUP >=    IF 4 . TESTFAIL THEN
    2DUP =     IF 5 . TESTFAIL THEN
    2DUP <> 0= IF 6 . TESTFAIL THEN
    2DROP ;
: TESTU< ( a b -- [ tests all U comparisons ] )
    2DUP U<  0= IF 7 .  TESTFAIL THEN
    2DUP U>     IF 8 .  TESTFAIL THEN
    2DUP U<= 0= IF 9 .  TESTFAIL THEN
    2DUP U>=    IF 10 .  TESTFAIL THEN
    2DUP =      IF 11 .  TESTFAIL THEN
    2DUP <>  0= IF 12 .  TESTFAIL THEN
    2DROP ;


HEX
( Signed comparisons: go by distance, so 7FFF<-7FFF, because delta is a-b = -2 )
1 2 <       TRUE EXPECT" 1<2"
7FFF 8000 < TRUE EXPECT" 7FFF<8000"
7FFF 8001 < TRUE EXPECT" 7FFF > -7FFF"
1  -1     > TRUE EXPECT" 1>-1"
-2 -1     < TRUE EXPECT" -2<-1"
-7FFF 2   > TRUE EXPECT" -7FFF>2"  ( wraps thru 0x8000, unsigned)
-7FF0 2   < TRUE EXPECT" -7FF0<2"  ( wraps thru 0, signed )

TEMPSTR" === TESTING 8000:\n" TELL
8000 0<   TRUE  EXPECT" 8000 0<"
8000 0<=  TRUE  EXPECT" 8000 0=<"
8000 0>   FALSE EXPECT" 8000 0>"
8000 0>=  FALSE EXPECT" 8000 0>="
( note: 0x8000 is INT_MIN, does weird stuff when distance is exactly 0 )
( FUCKY:
8000 0 <  TRUE  EXPECT" 8000 0 <"   ( is FALSE )
8000 0 <= TRUE  EXPECT" 8000 0 <="  ( is TRUE )
8000 0 >  FALSE EXPECT" 8000 0 >"   ( is FALSE )
8000 0 >= FALSE EXPECT" 8000 0 >="  ( is TRUE )
)

HEX
TEMPSTR" === NEAR ASSERTS (signed+unsigned):\n" TELL
( NEAR ASSERTS (all <, should work for both signed and unsigned) )
0 1       2DUP TEST< TESTU<
1 2       2DUP TEST< TESTU<
7FFE 7FFF 2DUP TEST< TESTU<
7FFF 8000 2DUP TEST< TESTU<
8000 8001 2DUP TEST< TESTU<
8001 8002 2DUP TEST< TESTU<
FFFE FFFF 2DUP TEST< TESTU<

( FAR COMPARISONS (unsigned only) )
TEMPSTR" === FAR ASSERTS (U):\n" TELL
1 FFFF TESTU<
0 7FFF TESTU<
0 8000 TESTU<
0 8001 TESTU<
1 8000 TESTU<
1 8001 TESTU<
8000 FFFF TESTU<


( FAR COMPARISONS (signed only), all < )
TEMPSTR" === FAR ASSERTS (signed):\n" TELL
0     7FFF TEST<
8001     0 TEST<
8000    -1 TEST<
-3000 3000 TEST<
D000  4000 TEST<
( FLIPPED, signed only )
5000 -5000 TEST<
6000  D000 TEST<
1     8000 TEST<


TEMPSTR" === TESTING other 0cmp\n" TELL
0001 0<   FALSE EXPECT" 0001 0<"
0001 0<=  FALSE EXPECT" 0001 0=<"
0001 0>   TRUE  EXPECT" 0001 0>"
0001 0>=  TRUE  EXPECT" 0001 0>="
0000 0<   FALSE EXPECT" 0000 0<"
0000 0<=  TRUE  EXPECT" 0000 0=<"
0000 0>   FALSE EXPECT" 0000 0>"
0000 0>=  TRUE  EXPECT" 0000 0>="
-1 0<   TRUE  EXPECT" -1 0<"
-1 0<=  TRUE  EXPECT" -1 0=<"
-1 0>   FALSE EXPECT" -1 0>"
-1 0>=  FALSE EXPECT" -1 0>="
7FFF 0<   FALSE EXPECT" 7FFF 0<"
7FFF 0<=  FALSE EXPECT" 7FFF 0=<"
7FFF 0>   TRUE  EXPECT" 7FFF 0>"
7FFF 0>=  TRUE  EXPECT" 7FFF 0>="
DECIMAL


FORGET' TESTCASE_START ( forget all our temporary testing words )


( ( =============== SIGNED DIVISION =============== )
( currently not working right: unsigned division seems to work correctly with positive
  divisors, but just negating stuff isn't working right anymore. Need to sit down and solve
  this properly at some point, but is not top priority )
( old solution was to flip both dividend and divisor positive, then flip back later?
  however this gave -quotient and -remainder. Might have been better to do -quotient and positive remainder?
  Need to think about proper conventions for negatives)

: /MOD ( a b -- )
    OVER 0>= [ LW_0BRANCH , 3 , ] ( if a positive, just do unsigned divide )
        U/MOD RETURN
    ( else )
    SWAP NEG SWAP U/MOD ( -quot, rem )
    SWAP NEG SWAP NEG ( negate quotient and remainder??? ) ;

: / ( a b - a/b )
    /MOD ( a/b a%b ) DROP ;
: MOD ( a b - a%b )
    /MOD ( a/b a%b ) NIP ;

)
0 VARIABLE V ( divisor )
: /SHOW ( n v -- )
    STR" ( " TELL
    ( n v ) 2DUP SWAP X. X. STR" /MOD -- ) " TELL
    DUP V !
    ( ( n v ) /MOD ) ( TODO: DEBUG: SIGNED? )
    ( n v ) U/MOD
    ( q r ) 2DUP SWAP  ( q r r q )
        C' Q EMIT X.
        C' R EMIT X.
    ( q r ) STR" Undoing: " TELL
    SWAP V @ * ( r q*v ) DUP X.
    SWAP ( q*v r ) STR" + " TELL DUP X.
    STR" = " TELL
    ( q*v r ) + X.
    ;

( ================== MISC TESTS  ====================== )

( let's test it by summing 0..2 * 10..12 )
( should be 0+0+10+11+20+22 = 63)
: TEST
    0 ( sum ) 3 0 DO 12 10 DO
        I> J> * +
    LOOP ( C' : EMIT DUP . NL ) LOOP ;
TEST 63 EXPECT" 2-dim loop"
FORGET' TEST

( ================== INTROSPECTION/DISASSEMBLY  ====================== )
HEX

: BETWEEN ( x a b -- 1/0 )
    ( return 1 if a < x < b )
    -ROT ( b x a )
    OVER < ( b x a<x )
    -ROT ( a<x b x )
    > ( a<x b>x )
    AND ;

( some regions of memory are memory-mapped control units, and even
  reading them can break stuff. Lets just say 0x01 to 0x04 are unsafe  )
: ISUNSAFEPTR ( ptr -- 1/0 )
    DUP 0 > SWAP 40 < AND ;

( some things we shouldn't even bother dissassembling, since all possible words
  are greater than this. Also ignore small negative integers )
( - stacks and stack pointers start at 0x200
  - forth inner interpreter starts at 0x800, main forth words soon after )
: BELOWALLWORDS ( ptr -- 1/0 )
    DUP -FF >= SWAP 200 <= AND ;

: -16ALIGN ( ptr -- aligned(ptr) ) FFF0 AND ; ( rounds down )
: 16ALIGN  ( ptr -- alignedptr ) 15 + -16ALIGN ; ( rounds up )
: -8ALIGN ( ptr -- a(ptr) ) FFF8 AND ;
: 8ALIGN  ( ptr -- a(ptr) ) 7 + -8ALIGN ;
DECIMAL

( basic: just prints them out )
: DUMP ( ptr len -- )
    OVER + SWAP ( ptrend ptr )
    ( Prints out len words starting at ptr
      pads to 5 digits (might collide if has -1000 ) )
    ?DO I> ISUNSAFEPTR IF
            C' ? EMIT 4 SPACES
        ELSE
            I> @ 5 U.R THEN
    ?LOOP ;

: HEXDUMP_HEADER
    7 SPACES ( skip corner )
    16 0 DO  ( columns at 5 char pitch: )
        C' x EMIT I> 4 U.R ( print xN, pad to 5 chars)
        I> 7 = IF 5 SPACES THEN ( add gap after 8th col )
    LOOP NL ;

: HEXDUMP ( ptr len -- )
    BASE @ HEX -ROT ( base ptr len -- ; save base )
    HEXDUMP_HEADER ( print out key along top of table )

    OVER + ( base ptr end )
    ( expand region to nearest multiple )
    16ALIGN SWAP -16ALIGN SWAP ( start end )

    SWAP ( end start ) DO
        ( print out "addr: val val val val..." )
        I> 5 U.R
        STR" : " TELL ( curr end )
        I> 8 DUMP    ( ; dump first 8 words )
        STR"  === " TELL
        I> 8 + 8 DUMP ( ; dump next 8 words )
        NL
    16 +LOOP
    BASE ! ( restore base );

( ==================  WORD INTROSPECTION/DISASSEMBLY  ====================== )

: ISWHA ( ptr -- )
    ( returns true if ptr could be a word address, i.e. flags is reasonable, has string )
    ( todo: check if cfa holds addr+1 (asm) or DO_COLON, DO_VAR, DO_CONST? won't
      work if add more interpreters though.
     check if link is also a wha? won't work recursively though, or might be 0 )
    ( rule out small addresses )
    STR" ISWHA NOT IMPL" TELL HANDLE_ERR ;


: CFA_MATCHES ( cfa ptr -- 1/0 )
    ( returns 1 if ptr points to the name string of cfa )
    ( strp@ gives len, strp+strp@ points to end of name, str+strp@+1 is cfa )
    DUP @ 1 ( cfa p p@ 1 )
    + + ( cfa strend+1 )
    = ;

 ( used when CFA couldn't find an address. Pushes its own cfa )
: CFA_ERR [ LW_LIT , WIP @ , ] ;
: CFA> ( cfa -- wha OR CFA_ERR )

    ( don't even bother checking for words if it's a
      really small address )
    DUP ABS 1000 < IF  ( cfa )
        DROP CFA_ERR RETURN
    THEN

    DUP 1- ( cfa ptr )
    32 ( max chars to search backwards )

    BEGIN ( cfa ptr i )
        >R ( cfa ptr ; stash i )

        2DUP CFA_MATCHES IF
            ( cfa ptr; ptr points to strlen field )
            NIP 2- ( skip strlen, flags )

            R> DROP ( get rid of i )
            RETURN ( returns wha )
        THEN

        ( if ptr contains anything other than ascii,
          we should early exit )
        DUP @ ISPRINTABLE 0= IF ( cfa ptr )
            R> DROP ( drop i from return stack )
            2DROP CFA_ERR RETURN  ( drop rest, return err )
        THEN


        1- ( cfa ptr-- )
        R> 1- ( check I )
    DUP 0<= UNTIL

    ( cfa ptr i )
    2DROP DROP
    CFA_ERR ; ( return err word )



: SHOW ( xt -- )
    ( tries to show a word a little bit? )
    BASE @ >R HEX ( stash base )

    DUP 4 U.R ( print word )
    STR" (" TELL

    DUP DO_COLON = IF ( xt )
        DROP STR" DO_COLON" TELL
    ELSE DUP LW_LIT = IF ( xt )
        DROP STR" LIT" TELL
    ELSE DUP LW_LITSTR = IF ( xt )
        DROP STR" LITSTR" TELL
    ELSE DUP LW_BRANCH = IF ( xt )
        DROP STR" BRANCH" TELL
    ELSE DUP LW_0BRANCH = IF ( xt )
        DROP STR" 0BRANCH" TELL
    ELSE DUP ISPRINTABLE IF ( if is ascii )
        C' ' EMIT EMIT C' ' EMIT
    ELSE DUP BELOWALLWORDS IF ( xt )
        DROP ( if small constant, just dont print anything )
    ELSE ( xt )
        CFA>  DUP CFA_ERR = IF
            DROP STR" ???" TELL
        ELSE
            >WNA TELL
        THEN
    THEN THEN THEN THEN THEN THEN THEN
    STR" )" TELL
    ( xt )

    R> BASE !  ( restore base )
    ;

: DISASSEMBLE ( addr len -- )
    BASE @ HEX -ROT
    OVER + SWAP ( end start )
    ?DO  I> 5 U.R  ( print addr: )
        STR" : " TELL
        I> @ SHOW NL ( show word at addr )
    ?LOOP
    BASE ! ;

: ISPRIMARY ( xt -- 1/0 )
    ( returns 1 if xt is a primary word, ie *CFA = CFA+1 )
    DUP @ 1- = ; ( cfa == *cfa-1 )

: SEE ( xt -- )
    DUP ABS 128 <  ( if xt small integer )
    OVER @ ABS 128 < OR ( or cw is small integer )
    IF
        STR" (not a word?)\n" TELL ( probably not a word )
        RETURN
    THEN
    ( TODO: integrate with show? so can use lost-words, etc )

    STR" === " TELL
    DUP SHOW NL ( print out "word (desc)" )

    DUP ISPRIMARY IF
        STR" ASM \n" TELL
    THEN

    ( xt ) 32 DISASSEMBLE
    ;

( ================== RETURN STACK INTROSPECTION/DISASSEMBLY  ====================== )

( first attempt )
: RSP@ ( -- rsp [at time of caller] )
    RSP_ @ NEG 1+ ; ( skip our own RSP entry? )


( local used in WRA> )
0 VARIABLE TESTRA

: ISRAINWORD ( laterwha currwha -- 1/0 ; [ra in TESTRA] )
    ( returns true if RA could be part of currwha word
      laterwha > currwha )
      >CFA SWAP ( currcfa laterwha )
      TESTRA @ -ROT ( ra currcfa laterwha )
      BETWEEN ;

( error sentinel for WRA> )
: WRA_ERR ( -- WHA:WRA_ERR ) [ WIP @ #, ] ; ( pushes own wha )

: WRA> ( ra -- wha [ WRA_ERR if not found )
    ( Given a return addr, find the word that it returns into )
    TESTRA ! ( stash RA )
    DP @ LATEST @ ( end_of_dict currwha )
    BEGIN DUP WHILE ( while currwha is not null )
        ( prevwha currwha )
        2DUP ISRAINWORD IF ( if this is it )
            NIP RETURN ( return currwha )
        THEN

        ( prev currwha ; else, move on to next word header )
        NIP DUP @ ( currwha nextwha )
        ( == prevwha currwha )
    WEND ( wha wha )
    2DROP WRA_ERR ;

: AB_TO_ALEN ( a b -- a b-a )
    OVER - ;


( ==== RSHOW: format return addresses informatively )
: RSHOW_1 ( ra wha - ra )
    ( prints "(WORD)+OFFSET", handles negative, handles WRA_ERR )
    '(' EMIT DUP >WNA TELL ')' EMIT  ( ra wha ; prints "(WORD)")
    DUP WRA_ERR = IF ( ra wha; if WRA_ERR, early exit )
        DROP RETURN ( ra )
    THEN
    ( ra wha ; calc and print offset: ra-start_of_word )
    OVER SWAP  ( ra ra wha ; stash a copy )
    >DFA -
    DUP 0>= IF C' + EMIT THEN ( add + sign? )
    4 U.R ; ( print out )

: RSHOW_2 ( ra -- ra )
    ( prints instruction at return address "= [ xxxx (WORD) ]" )
    STR" = [" TELL
    DUP @ SHOW  ( print using show )
    C' ] EMIT
    ;

: RSHOW ( ra -- )
    ( prints "ra: ", looks up WRA>, does RSHOW1 to print name and offset,
      then rshow_2 to print instruction at RA contents )
    DUP 4 U.R ( print the RA )
    C' : EMIT SPACE
    ( ra )
    DUP BELOWALLWORDS IF ( ra ; if it's a small integer, just exit here )
        DROP ( )
    ELSE ( ra; else figure out what word it returns into, code at that spot )
        DUP WRA> ( ra wha )
        RSHOW_1 ( ra )
        RSHOW_2 ( ra )
        DROP
    THEN  ;

: RSDUMP ( start last -- )
    BASE @ HEX >R
    1+ SWAP ( last+1 start ) ?DO
        ( print out: "{rsa}] {RSHOW}\n" )
        I> 3 U.R ')' EMIT SPACE
        I> @ RSHOW ( rsp;  fetch ra, format with rshow )
        NL
    ?LOOP
    R> BASE ! ;

: .RS ( -- prints return stack)
    RSP@ 1+ ( rsp ; ignore own RA )
    STR" === RSP: " TELL
    DUP U. NL ( rsp ; print rsp )
    ( rsp )
    RS0  ( rsp rs0 )
    RSDUMP ;

HEX
: .DS ( -- show the data stack a bit )
    SP@  ( sp )
    BASE @ HEX SWAP ( oldbase, sp )
    STR" ====== SP: " TELL
        DUP . NL
    10 - ( oldbase sp-0x10 )
    DS0 ( sp-0x10, DS0 )
    OVER - ( sp-0x10, len_to_DS0 )
    HEXDUMP ( show a bit on either side )
    BASE ! ;
DECIMAL

( === install new exception handler === )

: HANDLE_EXC ( rsp nwa cwa code -- )
    HEX
    DUP STR" Exception: " TELL . NL DROP
    STR" CWA: " TELL SHOW NL
    STR" NWA: " TELL RSHOW NL
    STR" === RSP: " TELL DUP . NL
    ( rsp )
    RS0 RSDUMP ( DUMP RETURN STACK )

    STR" === STACK) " TELL NL
    .S
    0 IN_EXC !
    HANDLE_ERR ;

' HANDLE_EXC EXC_HANDLER !


( TEMP TEST FUNCS )
: T3 1 >R  ;
: T2 RSP@ DEBUG @ ;
: T1 1 DUP T2 -ROT 2DROP ;
: WA' TOKEN UPPER FIND ;

( ========================================= )

: NUMWORDS ( -- n )
    0
    LATEST @ ( cnt wha )
    BEGIN DUP WHILE ( cnt wha ; while word is nonnull)
        SWAP 1+ ( increment )
        SWAP @  ( advance )
    WEND ( cnt wha )
    DROP ;

: GREET
    STR" == Core Forth Bootstrapping Complete ==" TELL NL
    SPACE NUMWORDS .
    STR" words defined" TELL NL
    STR"  DP: 0x" TELL
        BASE @ HEX DP @ . BASE ! ( save base, print DP in hex )
        NL
    STR"  Latest word: " TELL
        LATEST @ >WNA TELL NL
    ;


( ==== ASSERTS? NOT WORKING )

: ASSERT_INIT ( n -- [ saves 1 val to rs ] )
    ( we're saying "there's 1 thing on the stack rn, save that " )
    ( so, save stackcount -n (n itself will be baked in, but that's fine ) )
    STACKCOUNT SWAP - ( stackcount-n )
    R> SWAP >R >R ( push to return stack, under own RA )
    ;

: ASSERT ( n -- [ errors if needed ] )
   ( should error out if current STACKCOUNT-n differs from ASSERT_INIT time )
   STACKCOUNT SWAP - ( curr_stackcount-n )
   R> R> ( stack-n ra old_stack-n )
   DUP -ROT ( stack-n old_stack-n ra old_stack-n )
   >R >R ( stack-n old_stack-n ; push others back )
   - ( delta, how many more/less elements than expected )
   DUP 0<> IF ( if nonzero, error out )
           STR" STACK ASSERT ERR: " TELL
           ( delta )
           . NL ( print number and end line )
           HANDLE_ERR ( restarts )
   THEN ( delta )
   DROP ;



GREET
END_OF_FILE
