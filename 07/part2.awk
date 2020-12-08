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
    bag_holders[bag, innerbag] = count
  }
}
END {
  count = 0
  frontier[++frontier_len] = "shinygold"
  while (frontier_len > 0) {
    bag = frontier[frontier_len]
    delete frontier[frontier_len--]
    for (innerbag in bags) {
      bag_count = bag_holders[bag, innerbag]
      for (i = 0; i < bag_count; ++i) {
        frontier[++frontier_len] = innerbag
        count++
      }
    }
  }
  print count
}