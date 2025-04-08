START_LOADING_FILE
." Hello forth 3\n"

( counts how many bits to get to highest 1-bit
 i.e. 0->0, 0b1000->4, 0b1111->4 
 Doing `1  ( X LOG2 ) <<` will always be `X >`
 )
: ULOG2 ( u - u )
    0 SWAP BEGIN ( num_bits n )
    DUP 0<> WHILE
        ( num_bits n ) 1U>> SWAP 1+ SWAP
    WEND DROP ( num_bits ) ;

( initial guess for sqrt func: this one might undershoot )
: _SQRT0 ( u - n ) ULOG2 2/ 1 SWAP ( 1 log2 ) << ;

( this adds 1 before shifting, so is safe to use as a max for prime checking
    2 ceil_sqrt is 4, and 8 ceil_sqrt is 8, but it shouldn't overshoot
    for anything > 3 (5 and 7 are fine, and it quickly falls off after that))
: CEIL_SQRT ( u - u ) ULOG2 2/ 1+ 1 SWAP << ;



( ======== PRIMES ========= ) 

0 VALUE N
: DIVABLE ( n divisor -- bool ) MOD 0= ;

( returns whether N is divisible by any of start..lim,  by 2s )
: TRYDIVS ( stop start -- bool ; goes up by 2   ) 
    DO N I> DIVABLE IF 1 LOOPRETURN THEN 2 +LOOP 0 ;

: ODDCOMP ( n[odd] -- bool ) DUP TO N ( n ) 2/ 3 ( lim 3 ) TRYDIVS ;
: ODDCOMP2 ( n[odd] -- bool ) DUP TO N 
    ( n ) CEIL_SQRT 3 ( lim 3 ) TRYDIVS ;

: ISPRIME ( n )
    CASE 
        DUP 3 <=      IF DROP 1 ENDOF
        DUP 2 DIVABLE IF DROP 0 ENDOF
        DEFAULT ODDCOMP2 0= ENDOF
    ENDCASE ;

: LISTPRIMES ( stop start ) DO I> ISPRIME IF I> . THEN LOOP ;
: LISTPRIMES2 ( stop start[odd] ) DO I> ISPRIME IF I> U. THEN 2 +LOOP ;
." Here's some primes:\n"
100 2 listprimes NL
25100 25001 listprimes2 NL


: COLLATZ ( n ) DUP . ( print n )
CASE
    1 OF             .RS            ENDOF
    DUP 2 DIVABLE IF 2/ RECURSE     ENDOF
    DEFAULT          3 * 1+ RECURSE ENDOF
    ENDCASE ;

: TAIL_COLL ( n ) DUP . ( print n )
CASE
    1 OF             .RS            ENDOF
    DUP 2 DIVABLE IF 2/ TAILCALL RECURSE     ENDOF
    DEFAULT          3 * 1+ TAILCALL RECURSE ENDOF
    ENDCASE ;


: SQR ( n - n ) DUP * ;

0 VALUE TGT
: ERR ( guess - err ) SQR TGT ( guess^2 tgt ) - ;
: ADJUST ( guess err - guess2 ) OVER / 2/ ( guess err/2x ) - ;
: ITER ( guess - guess2 ) DUP ERR ( guess err ) ADJUST ;



( tgt is y, curr guess is x
  let err = x^2 - y   (as in: err>0 means x>sqrt(y))
  let rem = y-x^2     (as in: rem>0 means we need to go +, so x<sqrt(y))

  if err > 0, x > sqrt(y)
  if rem ==  2x+1, then y=(x+1)^2 = x^2+2x+1 = x^2+???
  if rem == -2x+1, then y=(x-1)^2 = x^2-2x+1 = x^2+???

  == swap to rem, flip order of rows
  if err ==  2x-1, then y=(x-1)^2 = x^2-2x+1 = x^2+???
  if err == -2x-1, then y=(x+1)^2 = x^2+2x+1 = x^2+???

  if rem == 2x+1: sqrt(y) == x+1, but iteration will catch it
  if rem > 0     && err < 2x+1,   sqrt(y) \isin (x, x+1), CEIL_OK
  if rem == 0                     sqrt(y) = x
  if rem > -2x+1 && err < 0,      sqrt(y) \isin (x-1, x), FLOOR_OK
  if rem == -2x+1, iteration will incorrectly go to x+1, which is wrong
  if rem == -2x, iteration gets it, which is fine
)
0 VALUE _2X
: ITER_REC ( guess - guess2 ) 
    DUP 2* TO _2X
    DUP ERR CASE ( guess rem )
        DUP _2X 1- > IF  ( > 2x-1, keep iterating )
            ( ." > |" .s nl )
            ( guess err ) ADJUST TAILCALL RECURSE ENDOF
        DUP 0> IF  ( 1..=2x-1, we're >sqrt(y), x-1 is floor(sqrt) )
            ( ." ceil |" .s nl )
            ( guess err ) DROP 1- RETURN ENDOF
        DUP _2X NEG 1- > IF  ( in {-2x-1,0], we're at floor(sqrt) )
            ( NOTE: if err exactly equaly to -2x+1, then  )
            ( ." flr |" .s nl )
            DROP ( guess ) RETURN ENDOF
        DEFAULT 
            ( ." < |" .s nl )
            ( guess err ) ADJUST TAILCALL RECURSE ENDOF
    ENDCASE ;
    
: SQRT ( n - r )
    TO TGT
    TGT _SQRT0 ITER ITER_REC ;


( BUG: mostly returns floor sqrt, but occasionally overshoots:
    e.g. 251^2 = 63001, but `63000 SQRT` gives 250, because
    error at is -500, which is exactly double the guess.
  To improve this, maybe stop when error is >0 and less than
  2*guess? (stop by subtracting 1? )
  Fixed with final_iter
)
( if err is in the range 1..2x, then floor(sqrt(y)) == x-1 
    because err = x^2 - y, and an err of 2x+1 means )
( if we've overshot (sqrt(y) is between x and x+1), subtract 1)
: FINALITER ( guess - guess2 ) 
    DUP 2* TO _2X
    DUP ERR ( guess err ) 1 _2X WITHIN= IF 1- THEN ;
: SQRT_OLD ( n - r )
    TO TGT
    TGT _SQRT0 ( first_guess )
    ITER ITER ITER FINALITER ;

: SHOWITER ( n - n ) ." G=" DUP . ." , ERR=" DUP ERR . NL ;
: SHOWSQRT ( n - r )
    TO TGT
    TGT _SQRT0 ( get initial guess )
    SHOWITER ITER 
    SHOWITER ITER 
    SHOWITER ITER 
    SHOWITER ITER 
    SHOWITER ITER 
    SHOWITER FINALITER
    SHOWITER 
    DROP ;


( ==== Let's write tests for these: make sure we actually have floor ===)
0 VALUE _RT
: CHECK_SQRT ( y -- )
    SQRT TO _RT
    _RT SQR TGT <=  ( rt should be <= )
    _RT 1+ SQR TGT > AND  ( 1+rt should be > )
    ( prints "y 1" or "y 0" )
    ( TGT U. _RT U. DUP . NL  )
    ;
: TEST_ROOTS DO I> CHECK_SQRT 0= IF ." ERR: " I> U. NL  THEN LOOP ;
( Run the tests: 
    ( can't combine these into one loop because loops use signed > q_q )
63000 33000 test_roots
31999 1 test_roots
)

( now let's iterate )

    

END_OF_FILE
