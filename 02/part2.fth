\ Hello world!

s" input" r/o open-file throw value input

256 constant buflen
create buf buflen allot

: parse-number ( c-addr1 u1 c-addr2 u2 -- n c-addr3 u3)
  rot drop \ fuck a u1
  -rot \ deal with u2 later ( u2 c1 c2 )
  dup -rot \ deal with c2 later ( u2 c2 c1 c2 )
  over - ( u2 c2 c1 c2-c1 )
  s>number? invert throw throw ( u2 c2 n )
  rot 1 - ( c2 n u3 )
  rot 1 + ( n u3 c3 )
  swap \ that wasn't so bad
;

: parse-line ( c-addr u -- p1 p2 char pass-addr u )
  2dup s" -" search invert throw ( c1 u1 c2 u2 )
    parse-number ( min c3 u3 )
    2dup s"  " search invert throw ( min c3 u3 c4 u4 )
    parse-number ( min max c5 u5 )
    over -rot ( min max char c5 u5 )
    swap 3 + swap 3 - ( yay )
;

: is-between ( min max n -- f )
  tuck >= -rot <= and invert
;

: is-valid ( c-addr u -- f )
  parse-line

  \ simple bounds check
  -rot 2swap 2dup
  > throw
  2swap rot drop

  ( p1 p2 char pass-addr )
  rot over + 1 - ( p1 char pass-addr p2+pass-addr-1 )
  rot dup -rot 1 swap 1 str= ( p1 pass-addr char p2good )
  2swap + 1 - rot 1 tuck str= ( p2good p1good )
  <>
;

: count-valid
0
begin
  buf buflen input read-line throw
while
  buf swap is-valid if
    1 +
  endif
repeat
drop
;

count-valid . cr