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
: DEPTH ( - num_items_on_stack)
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


: WITHIN ( x ul uh -- b )
    -ROT OVER ( b x a x )
    <= -ROT ( a<=x b x )
    > AND ;
: WITHIN= ( x ul uh -- b ; upper bound is inclusive) 1+ WITHIN ;


( convert ptr + len to loop bounds )
: PL>LB ( ptr len -- end start ) OVER + SWAP ;

( ============== SCRIBING WORDS =========== )

( In future we'll add error-handling to FIND so it can error out
 if not found. However we still want access to the base FIND
 for cases where we handle the returns-0 )
: ?FIND ( strp - [cfa or 0]) FIND ;
: FIND ( strp - cfa [ errors if not found]) FIND ;
( all subsequent uses of find will use this, which we'll patch to do
  errorchecking )

: HERE DP @ ;
: ALLOT DP +! ;
: #, ( n -- ; compiles TOS as a literal number ) LW_LIT , , ;
: ' ( - CFA ; lookup CFA of next token ) TOKEN FIND >CFA ;
: ['] ' #, ; IMMEDIATE
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


( ============= VARIABLES ============= )

: VARIABLE ( init_val [TOKEN] - )
    ( creates a new word that returns ptr to value)
    HERE         ( init_val hdr -- store current addr )
    TOKEN CREATE ( create header, links into latest )
    LATEST !     ( init_val -- : link word into latest)
    DO_VAR ,     ( set codefield as DO_VAR )
    ,            ( enclose initial value )
;

: CONSTANT ( init_val [TOKEN] - )
    ( creates a new word that returns value)
    HERE         ( init_val hdr -- store current addr )
    TOKEN CREATE ( create header, links into latest )
    LATEST !     ( init_val -- : link word into latest)
    DO_CONST ,   ( set codefield )
    ,            ( enclose initial value )
;

( addresses of ASM vars / labels )
HEX
800 CONSTANT $NWA
800 CONSTANT $NEXT
810 CONSTANT $CWA
810 CONSTANT $RUN
: 'EXIT [ ' EXIT #, ] ;
DECIMAL

( ============== COMPILE WORDS =========== )

( Define postpone, useful for making compiling words that
  use other compiling words at macroexpand time
  e.g. : WHILE [POSTPONE] IF ;
)
: [POSTPONE] ( <reads immediate word> -- <compiles it for later> )
    ' , ; IMMEDIATE

( compiles instructions to compile this word )
: [COMPILE]
    ' #, ( compile literal for the quoted token )
    ['] , , ( compile in a , to enclose it )
    ; IMMEDIATE

: RECURSE ( -- [compiles word being currently defined] )
    WIP @ >CFA , ; IMMEDIATE

( TAILCALL a word, i.e. the next word will not return to us, it will
    return to our caller )
: TAILCALL LW_TAILCALL , ; IMMEDIATE

( should function identically to exit, but has a different CFA,
  should help in disassembly )
: RETURN TAILCALL ; ( will tailcall its own exit )

( mark and resolve, mark leaves an addr, resolve fixes up an addr )
( to be used with branch/0branch. )
: CFMARK< ( -- tgtaddr ) HERE ;
: CFRESOLVE< ( tgtaddr -- ) HERE - ( delta tgt-here ) , ;
: CFMARK>    ( -- fixaddr ) HERE 1 ALLOT ;
: CFRESOLVE> ( fixaddr -- ) HERE OVER - ( fixaddr\delta ) SWAP ! ;


( ========== CONTROL FLOW ========= )

( BEGIN loopbody test UNTIL )
( BEGIN loopbody REPEAT )
: BEGIN  ( -- mark< ) CFMARK< ; IMMEDIATE
: UNTIL  ( mark< -- ) LW_0BRANCH , CFRESOLVE< ; IMMEDIATE
: REPEAT ( mark< -- ) LW_BRANCH , CFRESOLVE< ; IMMEDIATE


( test IF a ELSE b THEN )
( compiles to:
  test 0BRANCH to_else a BRANCH to_then offset b _
)
: IF ( -- mark> ) LW_0BRANCH , CFMARK> ; IMMEDIATE
: THEN ( if_fix | else_fix -- ) CFRESOLVE> ; IMMEDIATE

( compiles a BRANCH, marks>, will jump past the THEN )
( resolves IF's mark to 0BRANCH into the else case )
: ELSE ( if_fixup -- else_fixup )
    LW_BRANCH , ( enclose BRANCH )
    CFMARK> SWAP ( else_mark> if_mark> -- )
    CFRESOLVE>
    ; IMMEDIATE


( BEGIN test WHILE loop WEND )
( compiles to:
  begin:
    [test]
    0BRANCH to_wend
    [loopbody]
    BRANCH begin
  wend:
)
( NOTE: while compiles exactly like if)
: WHILE [POSTPONE] IF ; IMMEDIATE

( compiles a branch back to BEGIN,
  fixes up WHILE's offset to point after itself )
: WEND ( begin_mark< while_mark> -- )
    LW_BRANCH ,
    SWAP CFRESOLVE< ( WEND branches back to begin/loop-test )

    ( while_mark> -- ) CFRESOLVE> ( WHILE branches out of loop once test fails )
    ; IMMEDIATE


( ========== NESTED COMMENTS ========= )

: (
    1 ( nesting_depth )
    BEGIN DUP 0> WHILE ( loop until paren depth 0 )
        KEY ( depth char -- )
        DUP '(' = IF ( depth char )
            DROP 1+ ( depth+1 )
        ELSE DUP ')' = IF ( depth char )
            DROP 1- ( depth-1)
        ELSE DROP THEN THEN
        ( depth )
    WEND
    DROP ( )
    ; IMMEDIATE

( ( WOO HOO WE CAN DO NESTED ) )

( ============ DO/LOOP ============ )


( runtime: ( end start -- RS: end start ) )
: DO ( compile: -- DO_mark< ) LW_DO , CFMARK< ; IMMEDIATE

( for all 3: stack is ( DO_mark< -- ) )
: LOOP  1 #, LW_+LOOP , CFRESOLVE< ; IMMEDIATE
: +LOOP      LW_+LOOP , CFRESOLVE< ; IMMEDIATE
: -LOOP      LW_-LOOP , CFRESOLVE< ; IMMEDIATE

( ?DO ?LOOP compiles as `2DUP < IF DO .. LOOP ELSE 2DROP THEN` )
: ?DO ( -- marker-IF, marker-DO )
    [COMPILE] 2DUP [COMPILE] > [POSTPONE] IF
    [POSTPONE] DO
    ; IMMEDIATE
: ?+LOOP ( marker-IF marker-DO -- )
    [POSTPONE] +LOOP
    [POSTPONE] ELSE [COMPILE] 2DROP [POSTPONE] THEN
    ; IMMEDIATE
: ?LOOP ( marker-IF marker-DO -- )
    1 #, [POSTPONE] ?+LOOP
    ; IMMEDIATE

: FLIP?DO ( marker-IF marker-DO -- markerIF marker-DO )
    OVER 2 - ( addr of >, "2DUP > 0BRANCH IF_mark>"  )
    ['] < SWAP ! ( overwrite )
    ;
: ?-LOOP ( marker-IF marker-DO -- )
    FLIP?DO ( switch test to use < instead of > )
    [POSTPONE] -LOOP
    [POSTPONE] ELSE
        [COMPILE] 2DROP [POSTPONE] THEN
    ; IMMEDIATE


( : I> R@ ; )
: J> ( RS: jend j iend i ra -- ) 3 RPICK ;
: K> 5 RPICK ;

: LEAVE ( sets I = end )
    ( RS: end I ra ) R>
    R> DROP ( RS: end )
    R@ >R >R ( RS: end end ra ) ;

( ================== CASE...OF..ENDOF..ENDCASE ======================= )
( NOTE === HOW SHOULD DEFAULT BE HANDLED?
    X @ CASE
        1 OF ... ENDOF
    ENDCASE ( should drop X )
    X @ CASE
        1 OF ... ENDOF
        DEFAULT ." other: " .  ENDOF
    ENDCASE ( endcase still drops X, but we shouln't reach it? )
        ." other:" . DEFAULT ? ( cancels out the DROP? modifies ENDCASE? )
)

0 VARIABLE #CASES
: CASE ( saves #CASES, inits to 0 )
    #CASES @
    0 #CASES ! ; IMMEDIATE

: OF [COMPILE] OVER [COMPILE] = [POSTPONE] IF [COMPILE] DROP ; IMMEDIATE
: ENDOF [POSTPONE] ELSE   1 #CASES +! ; IMMEDIATE

( compiles #CASES THENs, restores #CASES )
: ENDCASE ( old_#case else_mark else_mark ... else_mark -- )
    [COMPILE] DROP
    #CASES @ 0 ?DO [POSTPONE] THEN ?LOOP
    #CASES ! ; IMMEDIATE

: DEFAULT ( used like 'DEFAULT (val) ... ENDOF ENDCASE' )
    1 #, [POSTPONE] IF ; IMMEDIATE

( ( == Example use: )
: TESTCASES
    ( x ) CASE
        1 OF ." 1:" ENDOF
        2 OF ." 2:" ENDOF
        3 OF ." 3:" ENDOF
        DUP ISPRINTABLE IF EMIT ENDOF
        DEFAULT ." def: " . ENDOF
    ENDCASE ;
)
( == equivalent to:
    ( x )
        1 OVER = IF DROP ... ELSE
        2 OVER = IF DROP ... ELSE
        3 OVER = IF DROP ... ELSE
        DUP ISASCII IF ... ELSE
        1 IF ." def: " . ELSE
        DROP
    THEN THEN THEN THEN
)

( === Alt definition for OF: more compact but slower
      if coded in ASM would be very fast.
: _OF OVER = IF
        DROP  ( drop switch var )
        R> 1+ >R ( skip branch )
    ELSE R> DUP @ + >R ( take branch, add offset )
    THEN ;
: OF ( -- marker_of> ) [COMPILE] _OF CFMARK> ; IMMEDIATE
)


( ========== MORE MATH HELPERS ========= )

: STREND ( strp -- endp(exc) ) DUP @ + 1+ ;
: STRBOUNDS ( strp -- &str_end &str[0] ) DUP STREND SWAP 1+ ;

: ABS DUP 0< IF NEG THEN ;

( ========== OUTPUT . and friends ========= )

: SPACES ( n -- [ outputs n spaces, min 0 ] )
    BEGIN DUP 0> WHILE SPACE 1-
    WEND ( n )
    DROP ;

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

: CSTACKSHOW C' : EMIT DEPTH DIGASCII EMIT SPACE  ;
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

: U. ( n -- ) 0 U.R SPACE ;

: SIGNR ( n width -- n w | -n w-1 ) OVER 0< IF C' - EMIT 1- SWAP NEG SWAP THEN ;
: .R ( number width -- [prints signed, padded to width] ) SIGNR U.R ;
: . ( number ) 0 .R SPACE ;
: ? ( addr -- ) @ . ;

DECIMAL
( . was bugged for a while with negatives and such, this was a stopgap measure: )
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
    DEPTH . C' : EMIT SPACE
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
: ISPRINTABLE ( c -- 1/0 ) 31 126 WITHIN= ;

( update C' to work in NORMAL mode as well )
: C' CHAR   STATE @ IF #, THEN ; IMMEDIATE

: ISASCIILOWER ( c -- bool ) DUP C' a >= SWAP C' z <= AND ;
: ASCIIUPPER DUP ISASCIILOWER IF [ 'A' C' a - #, ] + THEN ;
: UPPER ( strp -- strp [ uppercases the string] )
    DUP STRBOUNDS ?DO I> @ ASCIIUPPER I> ! ?LOOP ;

( === NOTE: actually I think maybe I don't want' this? cleaner to ['], and we need ' for compile words?
( update quote ' to be flexible, working in normal mode or compile mode
  also looks up by uppercasing )
: ' ( <finds next word> -- <pushes / compiles CFA > )
    TOKEN UPPER FIND >CFA ( lookup using old func )
    STATE @ IF #, THEN
    ; IMMEDIATE
)
: ' TOKEN UPPER FIND >CFA ;
: ['] ' #, ; IMMEDIATE


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
        [COMPILE] TELL
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


( duplicates if nonzero, useful for ?DUP IF )
: ?DUP  ( n -- [ n n OR 0 ] ) DUP 0> IF DUP THEN ;

( We wan't to behave differently when loading a forth file vs when in interactive mode
 In interactive mode, errors are reported immediately, and the rest of the line is skipped
 In reading_file mode, errors should break interpreting until the end of the file
)

0 VARIABLE BULKLOAD ( Set to 0 by RESTART, set to 1 for bulk-loading,
                      disables OK and PROMPT on newline )

( === FILE LOADING MODE: === )

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

: START_LOADING_FILE ( switches into bulkload mode )
    NL ." Loading until END_OF_FILE\n"
    1 BULKLOAD ! ;

: END_OF_FILE ( switches out of bulkload mode)
    ." Finished Loading\n"
    0 BULKLOAD ! ;

: LOAD_DEBUG ( skips rest of file, breaks out of bulk mode )
    SKIP_TO_EOF END_OF_FILE
    ." DEBUG: Skipped rest of file\n" ;


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
    DEPTH 1- .  ( subtract 1 to account for saved BASE )

    C' > EMIT SPACE
    BASE ! ( restore base )
    ;

: RESTART ( ??? )
    BASE @ 0= IF ( if starting first time )
        ." Interpreter v0.2: starting\n"
    ELSE ( restarting )
        ." RESTART (v0.2)\n"
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

    ." ERR: Returned from interpreter"
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
        ." ERROR LOADING FILE\n"
        ." - Last word was: "
            LATEST @ >WNA TELL NL ( print last word )
        ." - Skipped rest of file ("
            SKIP_TO_EOF
            . ." lines)\n"
        0 BULKLOAD ! ( switch back to interactive mode )
    ELSE SKIP_TO_NL

    THEN

    RESTART
    ;

: ?STACK ( checks stacks for underflow, errors out if found )
    DEPTH 0< IF
        ." DS UNDERFLOW: "
            DEPTH .
            ." items on stack" NL
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
                        SPACE ." OK" NL
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
    ." Upgrading interpreter to v0.2\n"

    ( ' NEW_INTERPRET INTERPETER ! ( patch in interp ) )
    ( Overwrite interpret with our new one )
    ['] INTERPRET ['] NEW_INTERPRET PATCH

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
        ." ERROR: STR\" only valid in compile mode\n" HANDLE_ERR
    THEN [POSTPONE] STR" ( call into old STR" )
    ; IMMEDIATE

( we can finally make FIND and friends take advantage of error checking )
: _CHECKED_FIND ( strp - cfa [ errors if not found ] )
    DUP ?FIND ?DUP IF
        ( strp cfa ) NIP RETURN ( -- cfa )
    THEN ( strp -- ; if not found: )
    ." ERROR IN FIND: " ( bad_strp ) TELL ." ?\n" HANDLE_ERR ;

' FIND ' _CHECKED_FIND PATCH ( patch default FIND )

( === stack pointer assert: )
: CSP_FAIL ( exp_depth -- )
    ." CSP ERROR: expected depth=" . NL
    ." Got depth=" .S HANDLE_ERR ;

0 VARIABLE CSP:
0 VARIABLE CSP ( for manual use )
: CSP! DEPTH CSP ! ;
: CSP? DEPTH CSP @ <> IF CSP @ CSP_FAIL THEN ;

( Upgrade : to error out if we call it in compile mode )
: : ( does normal : things )
    STATE @ IF
        ." ERROR: : called while already in compile mode\n" HANDLE_ERR THEN
    ( defer to old colon )
    [POSTPONE] :
    DEPTH CSP: !
    ; IMMEDIATE

: ;
    STATE @ 0= IF
        ." ERROR: ; called while not in compile mode\n" HANDLE_ERR THEN
    DEPTH CSP: @ <> IF CSP: @ CSP_FAIL THEN
    [POSTPONE] ;
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
    ." \n === ENTERING DEBUGGER ===\n"
    .S
    NEW_INTERPRET
    ." \n === LEAVING DEBUGGER ===\n"
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
        ." ERROR ("
        ROT TELL ( ; print strp )
        ." ): EXPECTED "
        ( a exp )
        .  ( a; print exp)
        ." , GOT "
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
    ." UNSIGNED: a<b b<a a<a b<b: "
    ( a b )
    2DUP U< TF
    2DUP SWAP U< TF
    ( a b ) OVER DUP U< TF
    ( a b ) DUP U< TF ( a )
    DROP ;

: TESTFAIL ( a b -- [prints failing case, errs out] )
    ." TEST FAIL: ("
        2DUP SWAP X. X. ." -- ): "
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

." === TESTING 8000:\n"
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
." === NEAR ASSERTS (signed+unsigned):\n"
( NEAR ASSERTS (all <, should work for both signed and unsigned) )
0 1       2DUP TEST< TESTU<
1 2       2DUP TEST< TESTU<
7FFE 7FFF 2DUP TEST< TESTU<
7FFF 8000 2DUP TEST< TESTU<
8000 8001 2DUP TEST< TESTU<
8001 8002 2DUP TEST< TESTU<
FFFE FFFF 2DUP TEST< TESTU<

( FAR COMPARISONS (unsigned only) )
." === FAR ASSERTS (U):\n"
1 FFFF TESTU<
0 7FFF TESTU<
0 8000 TESTU<
0 8001 TESTU<
1 8000 TESTU<
1 8001 TESTU<
8000 FFFF TESTU<


( FAR COMPARISONS (signed only), all < )
." === FAR ASSERTS (signed):\n"
0     7FFF TEST<
8001     0 TEST<
8000    -1 TEST<
-3000 3000 TEST<
D000  4000 TEST<
( FLIPPED, signed only )
5000 -5000 TEST<
6000  D000 TEST<
1     8000 TEST<


." === TESTING other 0cmp\n"
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
    ." ( "
    ( n v ) 2DUP SWAP X. X. ." /MOD -- ) "
    DUP V !
    ( ( n v ) /MOD ) ( TODO: DEBUG: SIGNED? )
    ( n v ) U/MOD
    ( q r ) 2DUP SWAP  ( q r r q )
        C' Q EMIT X.
        C' R EMIT X.
    ( q r ) ." Undoing: "
    SWAP V @ * ( r q*v ) DUP X.
    SWAP ( q*v r ) ." + " DUP X.
    ." = "
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

: SMALLINT? ( n -- b ) -128 128 WITHIN ;
HEX
( some regions of memory are memory-mapped control units, and even
  reading them can break stuff. Lets just say 0x01 to 0x40 are unsafe  )
: ISUNSAFEPTR ( ptr -- 1/0 ) 8 21 WITHIN ;

( some things we shouldn't even bother dissassembling, since all possible words
  are greater than this. Also ignore small negative integers )
( - stacks and stack pointers start at 0x200
  - forth inner interpreter starts at 0x800, main forth words soon after )
: BELOWALLWORDS ( ptr -- b ) -0FF 200 WITHIN ;

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
        ." : " ( curr end )
        I> 8 DUMP    ( ; dump first 8 words )
        ."  === "
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
    ." ISWHA NOT IMPL" HANDLE_ERR ;


( returns 1 if ptr points to the name string of same word as cfa )
: CFA_MATCHES ( cfa strp? -- b ) STREND ( cfa potential_cfa ) = ;

 ( used when CFA couldn't find an address. Pushes its own cfa )
: CFA_ERR [ WIP @ #, ] ;
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
    ." ("

    DUP DO_COLON = IF ( xt )
        DROP ." DO_COLON"
    ELSE DUP LW_LIT = IF ( xt )
        DROP ." LIT"
    ELSE DUP LW_LITSTR = IF ( xt )
        DROP ." LITSTR"
    ELSE DUP LW_BRANCH = IF ( xt )
        DROP ." BRANCH"
    ELSE DUP LW_0BRANCH = IF ( xt )
        DROP ." 0BRANCH"
    ELSE DUP LW_DO = IF ( xt )
        DROP ." DO"
    ELSE DUP LW_+LOOP = IF ( xt )
        DROP ." +LOOP"
    ELSE DUP LW_-LOOP = IF ( xt )
        DROP ." -LOOP"
    ELSE DUP $NEXT = IF ( xt )
        DROP ." $NEXT"
    ELSE DUP $RUN = IF ( xt )
        DROP ." $RUN"
    ELSE DUP ISPRINTABLE IF ( if is ascii )
        C' ' EMIT EMIT C' ' EMIT
    ELSE DUP SMALLINT? IF ( if is a non ascii number )
        SPACE .
    ELSE DUP BELOWALLWORDS IF ( xt )
        DROP ( if small constant, just dont print anything )
    ELSE ( xt )
        CFA>  DUP CFA_ERR = IF
            DROP ." ???"
        ELSE
            >WNA TELL
        THEN
    THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
    ." )"
    ( xt )

    R> BASE !  ( restore base )
    ;

: PREVWORD ( addr - xt ) 1- @ ;
: DS_OFFSET? ( addr - b ) PREVWORD
    DUP LW_BRANCH  = IF DROP TRUE RETURN THEN
    DUP LW_0BRANCH = IF DROP TRUE RETURN THEN
    DUP LW_+LOOP   = IF DROP TRUE RETURN THEN
    DUP LW_-LOOP   = IF DROP TRUE RETURN THEN
    DROP FALSE RETURN ;
: DS_LITVAL? ( addr - b ) PREVWORD LW_LIT = ;

: SHOWOFFSET ( addr - ; shows offsets like "4   (->3BFe)" )
    DUP @ 4 U.R
    DUP @ 0> IF ." (->" ELSE ." (<-" THEN
    DUP @ + ( jump_tgt ) 4 U.R ." )" ;

: DIS1 ( addr -- )
    DUP 5 U.R ." : " ( print addr: )
    DUP ISUNSAFEPTR IF DROP ." unsafe\n" RETURN THEN
    DUP DS_OFFSET? IF
        ( addr ) SHOWOFFSET NL
    ELSE
        ( addr ) @ SHOW NL
    THEN
    ;

: DISASSEMBLE ( addr len -- )
    BASE @ HEX -ROT
    OVER + SWAP ( end start )
    ?DO I> DIS1 ?LOOP
    BASE ! ;

'EXIT VARIABLE DIS_END ( stop when we hit this )
48 VARIABLE DIS_MAX ( max length to disassemble )

: SAFE@ ( p - v ) DUP ISUNSAFEPTR IF DROP 0 ELSE @ THEN ;
: DIS_TO_END ( addr -- )
    BASE @ HEX SWAP
    DIS_MAX @ PL>LB ( end start ) DO
        I> DIS1
        I> SAFE@ DIS_END @ = IF LEAVE THEN
    LOOP

    BASE ! ;


: NOTWORD ( xt -- b ) DUP SMALLINT? SWAP @ SMALLINT? OR ; ( xt small int or cw small int )
: ISPRIMARY ( xt -- 1/0 )
    ( returns 1 if xt is a primary word, ie *CFA = CFA+1 )
    DUP @ 1- = ; ( cfa == *cfa-1 )
: SEE ( xt -- )
    ." === " DUP SHOW ( print out "=== word (desc)" )
    DUP NOTWORD IF ." : not a word?\n" ( xt ) DROP RETURN THEN
    DUP ISPRIMARY IF ." : ASM" $NEXT DIS_END ! ELSE 'EXIT DIS_END ! THEN
    NL ( xt ) DIS_TO_END
    ;

( ================== RETURN STACK INTROSPECTION/DISASSEMBLY  ====================== )

( first attempt )
: RSP@ ( -- rsp [ of caller] )
    RSP_ @ NEG 1+ ; ( skip our own RSP entry? )


( local used in WRA> )
0 VARIABLE TESTRA
: ISRAINWORD ( laterwha currwha -- 1/0 ; [ra in TESTRA] )
    ( returns true if RA could be part of currwha word
      laterwha > currwha )
      >CFA SWAP ( currcfa laterwha )
      TESTRA @ -ROT ( ra currcfa laterwha )
      WITHIN ;

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
    ." = ["
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

: .RS ( -- [prints return stack])
    RSP@ 1+ ( rsp ; ignore own RA )
    ." === RSP: " DUP U. NL
    ( rsp ) RS0 RSDUMP ;

HEX
: .DS ( -- show the data stack a bit )
    SP@  ( sp )
    BASE @ HEX SWAP ( oldbase, sp )
    ." ====== SP: "
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
    DUP ." Exception: " . NL DROP
    ." CWA: " SHOW NL
    ." NWA: " RSHOW NL
    ." === RSP: " DUP . NL
    ( rsp )
    RS0 RSDUMP ( DUMP RETURN STACK )

    ." === STACK) " NL
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
    ." == Core Forth Bootstrapping Complete ==" NL
    SPACE NUMWORDS .
    ." words defined" NL
    ."  DP: 0x"
        BASE @ HEX DP @ . BASE ! ( save base, print DP in hex )
        NL
    ."  Latest word: "
        LATEST @ >WNA TELL NL
    ;

( ===================== OUTPUT SELF AS A BINARY ==================== )

0 VARIABLE CURRP ( current output-binary offset in words )
: B, ( n -- [ emits n as LE-binary ] )
    CSPLIT EMIT EMIT 1 CURRP +! ;


: ASSERTLE ( addr -- ; ) DUP CURRP @ SWAP ( addr currp addr ) > IF
        HEX ." BINERROR: currp= " CURRP ?
        ." , expected  <= " ( addr ) . NL
    ELSE DROP THEN ;
: ASSERTEQ ( addr -- ) DUP CURRP @ <> IF
        HEX ." BINERROR: currp=" CURRP ?
        ." , expected " ( addr ) . NL
        HANDLE_ERR
    ELSE DROP THEN ;


HEX
( : BECHO ( -- ; outputs whatever is at currp ) CURRP @ @ B, ; )
: BDUMPTILL ( end -- ; ) DUP ASSERTLE CURRP @ ?DO I> @ B, ?LOOP ;
: BPADTILL ( end padval -- ; )  SWAP ( pad end ) DUP ASSERTLE CURRP @ ?DO DUP B, ?LOOP DROP ;
: BLANKTILL ( end -- ; )  000D BPADTILL ;



( constants for binary output )
2 CONSTANT $ENTRY ( address of entrypoint )

: B$?   ( -- )      CURRP @ 1+ B, ; ( asm `?` )
: B$OP  ( a b -- )  SWAP B, B, B$? ; ( asm `a b ?` )
: B$JMP ( addr -- ) 0 B, 0 B, ( addr ) B, ; ( asm `Z Z addr` )


: SETUPENTRY 0 ASSERTEQ 0 B, 0 B, 2000 B, ;

( see sforth.asm2 for logic:
    if we jump to 0D, it goes to 140
    if we execute bad mem we clear 0D then jump to 0D
    if we treat 0D as an XT, we jump to 150 )
: BADMEMCATCHER
    000D ASSERTEQ
    0150 B, 0 B, 0140 B, ;
DECIMAL

HEX

: MAKESTACKS
    0200 ASSERTEQ
    ( TOS, NOSP_, RSP_ live here )
    ( leave stacks blank, restart will fix canaries )
    0500 BLANKTILL ( blank vars, blank stacks, token buff )
    ;


: MAKECONSTS 0040 BLANKTILL ( integer constants in x40 - xA0 )
    0060 BDUMPTILL ( pos/neg ints 0-F )
    0080 BLANKTILL 00A0 BDUMPTILL ( pos/neg 0? - F? )
    ;

: MAKEINIT 2000 BLANKTILL ( asm entrypoint )
    $ENTRY $ENTRY B$OP   05D $ENTRY B$OP ( overwrite entry with 0D )
    ['] START 1+ B$JMP ( jump to body of START )
    ;

: MAKEBINARY
    SETUPENTRY
    0008 BLANKTILL
    000D 0 BPADTILL ( alu )
    BADMEMCATCHER
    0030 0 BPADTILL ( regs )
    0 B, ( C_, last char read )
    1 B, ( C_needs_fetch (start with no char) )
    MAKECONSTS
    0200 BDUMPTILL ( echo everything until stack (exceptions) )
    MAKESTACKS
    2000 BDUMPTILL ( emit core ( TODO: cut out tests? old init? ) )
    ( set up entry
        -don't need to init DP, LATEST, EXC_HANDLER.
        -restart_wd_addr is only for v0 interpreter )
    MAKEINIT

    3000 BLANKTILL ( empty space)
    DP @ BDUMPTILL ( copy dict )
    ;
DECIMAL

: SELFCOMPILE
    0 BULKLOAD ! ( reset this, not a good way to do it otherwise )
    MAKEBINARY
    0 HALTN ;

: TEST ; ( test word to catch if we're still in compile mode ) FORGET' TEST
 GREET
( SELFCOMPILE )
END_OF_FILE
