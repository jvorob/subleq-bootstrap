: IMMEDIATE 1 LATEST @ 1+ ! ;
: [ 0 STATE ! ; IMMEDIATE
: ] 1 STATE ! ;

: CHAR TOKEN 1+ @ ;
: C' LW_LIT , CHAR , ; IMMEDIATE
: (  KEY C' ) =
    [ LW_0BRANCH , -5 , ] ; IMMEDIATE
( We have comments now! (not nested though )
( ======== TODOS:
- STR",
- FIX QUIT/RESTART [fix base, RSP, ???, set canaries]
- Write ERRCHECK
    - Check stack underflow, rstack underflow
    - Check canaries
    - Error out? [reset data stack]
- Write prompt?
    - switch token to check for \n?
    - would need to detect compile mode? [no prompt while compiling]
    - would need some way to disable it while loading
    - EN_PROMPT NO_PROMPT ?
    - Also: error recovery changes: if error during NO_PROMPT, halt
    - global error handling?
        - e.g. IMMED_ONLY, that prints an error and calls int ERR? QUIT?
- Write UPPER? something that's uppercasing insensitive
- Write new interpreter:
    - Prompt (if not compiling, if not disabled]
    - Error check after word, error messages, QUIT/RESTART
    - newline handling?
- Startup text [ "x words loaded, y cells free" ]
- write DUMP
    - probably needs U.R

 Less urgent
 - [old] [new] PATCH_WORD
 - Add forth version constants?
 - fix unsigned . [e.g. FFFF U.]
 - VALUE / TO VALUE?
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
: 'a' C' a ;
: '0' C' 0 ;
: '(' C' ( ;
: ')' C' ) ;
: '"' C' " ;
: '\n' 10 ;


( Stack helpers )
: NIP ( a b - b ) SWAP DROP ;

: HEX 16 BASE ! ;
: DECIMAL 10 BASE ! ;

: @! @ ! ;

( ==== Compilation Helpers )

( Lookup next word's CFA )
: ' ( - CFA ; lookup CFA of next token ) TOKEN FIND >CFA ;
: #, ( n -- ; compiles TOS as a literal number ) LW_LIT , , ;
( wrinkle: does , mean "write this value into DP"
           or does it mean "compile this value"

    e.g. ' SOMEWORD ,
         5 #,
         FETCHSTR STR,  [ adds LITSTR? ]
)


: ALLOT DP +! ;

: >WNA ( WHA -- strp ) 2+ ;  ( gives str pointer to a word's name )

( Recurse into current word )
: RECURSE WIP @ >CFA , ; IMMEDIATE

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
: WHILE [ ' IF , ] ; IMMEDIATE

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


( ========== OUTPUT . and friends ========= )


: ABS DUP 0< IF NEG THEN ;

: DIGASCII ( dig -- [outputs ascii] )
    DUP 36 >= IF ( n )
        DROP C' ? EXIT
    THEN DUP 10 >= IF ( n )
        10 - 'A' + EXIT ( digit in 10-35, A-Z )
    THEN DUP 0 >= IF ( n )
        '0' + EXIT ( digit in 0-9 )
    THEN  ( n )
        DROP C' ?
    ;


: U.  ( number -- ; prints out unsigned )
    ( we're going to push the digits onto the stack, LSD on bottom )

    -1 SWAP  ( push -1 as sentinel )
    BEGIN ( -1 [..digits] rest )
        BASE @ /MOD ( -1 [digits] rest digit )
        SWAP        ( -1 [digits] digit rest )
    DUP 0= UNTIL ( if rest = 0, break )

         ( -1 [digits] 0)
    DROP ( -1 [digits] )

    ( we're guaranteed at least one digit)
    BEGIN
        ABS DIGASCII EMIT
    DUP 0< UNTIL ( stop when we hit the -1 )
    DROP  ( drop the -1 )
    SPACE ;

: . ( number )
    DUP 0< IF
        C' - EMIT ( print '-' )
        NEG  ( negate number )
    THEN U. ;

: ? ( addr -- ) @ . ;

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

    SP@ ( pointer to last thing we want to print)

    DS0 ( ptr_tos ptr_0 )

    BEGIN ( tos curr )
    2DUP <= WHILE ( TOSP curr ; stop once we pass TOS  )
        DUP @ . ( TOSP curr ; print curr )
        1- ( TOSP next )
    WEND ( TOSP next )
    2DROP NL ;


( ========== Fancier quoting/inlining ========= )

( SKIP THIS FOR NOW
( update quote ' to be flexible, working in normal mode or compile mode)
: ' ( <finds next word> -- <pushes / compiles CFA > )
    ' ( lookup using old func )
    STATE @ IF #, THEN
    ; IMMEDIATE

( Define postpone, useful for making compiling words that
  use other compiling words at macroexpand time )
: [POSTPONE] ( <reads immediate word> -- <compiles it for later> )
    ' , ;


: CONSTEXPR;
    [ ' STATE , ]
    [ ' @ , ]

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

)

( ========== MISC ========= )
( - STATUS?:show DP, LATEST, SP RSP, etc
  - nested comments
)

( TODO
: MEMWRITE" ( pointer -- [reads string into memory at specified location] )

: TELL" ( -- ; VERSATILE
        in compile: compiles string literal into word and tells it
        in normal mode:
)
)

: STR", ( -- strp ; Reads a string using KEY, encloses in dict, stops at " )
    DP @    ( strp ; save a ptr to start of the string )
    1 ALLOT ( ; leave room for the length field )
    0       ( strp len )
    KEY DROP ( Drop the initial space )
    KEY ( ; get first char )

    ( strp len char )
    BEGIN DUP '"' <> WHILE ( loop until we hit a " )
        ,  ( strp len ; enclose char)
        1+ ( strp len+1 )
        KEY ( get next key )
    WEND  ( strp len char )

    DROP  ( strp len )
    ( now, write length to str base )
    OVER !  ( strp )
    ; ( return strp )

: STR" ( -- ; quotes a string, encloses in word as a literal )
    ( IMMED_ONLY )
    LW_LITSTR ,
    STR",  ( enclose string in dict )
    DROP ( STR", returns a string pointer??? TODO: RETHINK THIS)
    ; IMMEDIATE



( ========== NEW OUTER INTERP, restart ========= )

: ?EXECUTE ( wha -- ; compiles or executes, based on STATE and ?IMMED)
    STATE @ IF ( we're in compile mode )

        ( it's immediate, execute it )
        DUP ?IMMED IF
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
    NL STR" Loading until END_OF_FILE" TELL NL
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
        DUP 1+ @ '\n' = IF ( if is \n )
            SWAP 1+ SWAP
        THEN
        STR" END_OF_FILE" STR= ( if is EOF, break )
    UNTIL
    ( -- num_skipped ) ;

: SKIP_TO_NL ( discards rest of input line until it hits a \n )
    BEGIN KEY '\n' = UNTIL ;


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

    ( reset variables )
    0 BULKLOAD ! ( ; interactive )
    10 BASE !    ( ; decimal )

    INTERPETER @ EXECUTE
    STR" ERR: Returned from interpreter" TELL
    ;

: HANDLE_ERR
    ( we had a stack underflow or undefined word. If in compile mode,
     reset WIP word)

    DECIMAL ( return to known state )

    STATE @ IF
        ( TODO: clear out WIP word )
        0 WIP !
    THEN

    0 STATE ! ( switch out of compile mode )

    ( If we were bulk-loading a file, errors should-early exit the whole thing)
    BULKLOAD @ IF
        STR" ERROR LOADING FILE " TELL NL
        STR" - Last word was: " TELL
            LATEST @ >WNA TELL NL ( print last word )
        STR" - Skipped rest of file (" TELL
            SKIP_TO_EOF
            . STR" lines)" TELL NL
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
    TOKEN

    DUP FIND ( tok wha|0 )
    ?DUP IF ( ; found word )
        ( tok wha )
        SWAP DROP ( wha )

        ( if we're in compile mode and word is not immediate, compile it )
        ?EXECUTE

        ?STACK
        ( ?STACK - check stacks: if error, discard input, reset )

    ELSE  ( tok )
        DUP NUMBER ( tok 0 | tok n 1 )
        IF
            ( tok n )
            NIP ( n )
            STATE @ IF #,
            THEN ( if normal mode, leave n on top of stack )


        ELSE ( either \n or invalid token )
            ( tok -- )
            DUP 1+ @ '\n' = IF
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

( Patch in new interpreter, switch to bulk load mode )
: TEMP STR" Upgrading interpreter to v0.2" TELL NL ; TEMP
( FORGET TEMP )
' NEW_INTERPRET INTERPETER ! ( patch in interp )
0 BASE ! ( clear base so we hit the START logic )
RESTART START_LOADING_FILE ( restart into new interp, switch to load mode )

( ================== INTROSPECTION ====================== )


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

( ==== ??? )

GREET
END_OF_FILE
