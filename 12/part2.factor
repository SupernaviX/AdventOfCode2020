USING: accessors arrays combinators fry io kernel locals math math.parser prettyprint sequences strings ;
IN: part2
TUPLE: command { dir string } { amount integer } ;
TUPLE: state { waypoint pair initial: { 10 1 } } { position pair initial: { 0 0 } } ;

: >command ( dir length -- command ) command boa ;
: <state> ( -- state ) state new ;
: >state ( waypoint position -- state ) state boa ;

: normalize-angle ( angle -- angle-from-0-to-360 )
    360 mod 360 + 360 mod ;

: distance>> ( state -- distance ) position>> [ abs ] map sum ;

:: rotate-coordinates ( degrees x y -- x y )
    degrees {
      { 0 [ x y ] }
      { 90 [ y x neg ] }
      { 180 [ x neg y neg ] }
      { 270 [ y neg x ] }
    } case ;

: move-waypoint ( state delta -- state )
    swap dup waypoint>> pick
    [ + ] 2map
    swap position>> >state
    nip ;

: rotate-waypoint ( state degrees -- state )
    over waypoint>>
    first2 rotate-coordinates 2array
    swap position>> >state ;

: move-forward ( state units -- state )
    over waypoint>> swap '[ _ * ] map
    swap dup position>> pick
    [ + ] 2map
    swap waypoint>> swap >state
    nip ;

: obey ( state command -- state )
    dup amount>> swap dir>> {
      { "N" [ 0 swap 2array move-waypoint ] }
      { "S" [ neg 0 swap 2array move-waypoint ] }
      { "E" [ 0 2array move-waypoint ] }
      { "W" [ neg 0 neg 2array move-waypoint ] }
      { "L" [ neg normalize-angle rotate-waypoint ] }
      { "R" [ rotate-waypoint ] }
      { "F" [ move-forward ] }
    } case ;


: parse-line ( line -- command )
    dup 1 head swap rest-slice string>number >command ;

: read-input ( -- seq ) lines [ parse-line ] map ;

read-input <state> [ obey ] reduce distance>> .