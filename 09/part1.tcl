proc map {fun list} {
  set res {}
  foreach element $list {lappend res [$fun $element]}
  return $res
}

proc is_valid {number seen} {
  set res {}
  set sorted [lsort -integer $seen]
  foreach first $sorted {
    if {$first > $number} {
      break
    }
    foreach second $sorted {
      if {$first + $second == $number} {
        return 1
      }
      if {$first + $second > $number} {
        break
      }
    }
  }
  return 0
}

global seen
set seen {}
set seen_idx 0
proc check {item} {
  set preamble 25
  global seen
  global seen_idx
  if {[llength $seen] < $preamble} {
    lappend seen $item
  } else {
    set valid [is_valid $item $seen]
    if {!$valid} {
      return 0
    }
    lset seen $seen_idx $item
    incr seen_idx
    if {$seen_idx == $preamble} {
      set seen_idx 0
    }
  }
  return 1
}

set fp [open "input" r]
set input [split [read $fp] "\n"]
close $fp

foreach item $input {
  set test [check $item]
  if {!$test} {
    puts $item
    break
  }
}
