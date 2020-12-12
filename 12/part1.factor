USING: accessors arrays combinators io kernel locals math math.parser prettyprint sequences strings ;
IN: part1
TUPLE: command { dir string } { amount integer } ;
TUPLE: state { heading integer } { position pair initial: { 0 0 } } ;

: >command ( dir length -- command ) command boa ;
: <state> ( -- state ) state new ;
: >state ( heading position -- state ) state boa ;

: normalize-angle ( angle -- angle-from-0-to-360 )
    360 mod 360 + 360 mod ;

: direction>> ( state -- dir )
    heading>> {
      { 0 [ "E" ] }
      { 90 [ "S" ] }
      { 180 [ "W" ] }
      { 270 [ "N" ] }
    } case ;

: distance>> ( state -- distance ) position>> [ abs ] map sum ;

: move ( state delta -- state )
    swap dup position>> pick
    [ + ] 2map
    swap heading>> swap >state
    nip ;

: rotate ( state deg -- state )
    swap dup heading>> pick
    + normalize-angle
    swap position>> >state
    nip ;

: obey ( state command -- state )
    dup amount>> swap dir>> {
      { "N" [ 0 swap 2array move ] }
      { "S" [ neg 0 swap 2array move ] }
      { "E" [ 0 2array move ] }
      { "W" [ neg 0 neg 2array move ] }
      { "L" [ neg rotate ] }
      { "R" [ rotate ] }
      { "F" [ over direction>> swap >command obey ] }
    } case ;

: parse-line ( line -- command )
    dup 1 head swap rest-slice string>number >command ;

: read-input ( -- seq ) lines [ parse-line ] map ;

read-input <state> [ obey ] reduce distance>> .