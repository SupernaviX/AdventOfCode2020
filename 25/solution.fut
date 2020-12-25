let step (sn: u64) (x: u64): u64 =
  (x * sn) % 20201227

let transform (sn: u64) (ls: i32) =
  loop x = 1 for i < ls do step sn x

let findls (sn: u64) (pk: u64): i32 =
  let i = 0i32
  let x = 1u64
  let (i, _) = loop (i, x) while pk != x do (i + 1, step sn x)
  in i

let main (cardkey: u64) (doorkey: u64) =
  let cardls = findls 7 cardkey 
  in transform doorkey cardls