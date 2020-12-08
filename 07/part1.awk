#!/bin/awk -f
{
  bag = $1 $2
  bags[bag] = true
  # $3 and $4 are always "bags contain"
  for (i = 5; i < NF; i += 4) {
    if ($i == "no") {
      break
    }
    count = $i
    innerbag = $(i+1) $(i+2)
    bag_holders[bag, innerbag] = bag_count
  }
}
END {
  count = 0
  frontier[++frontier_len] = "shinygold"
  while (frontier_len > 0) {
    innerbag = frontier[frontier_len]
    delete frontier[frontier_len--]
    for (bag in bags) {
      if ((bag, innerbag) in bag_holders && !(bag in seen)) {
        frontier[++frontier_len] = bag
        seen[bag] = true
        count++
      }
    }
  }
  print count
}