readfile =: 1!:1
lines =: LF splitstring readfile <'input'
numbers =: 0 ". / "1 > lines                  NB. prase input to string
numbers =: numbers, 0, 3 + >./ numbers        NB. add 3+max and 0 to numbers
sorted =: /:~numbers

at =: monad : 0
  if. (y < 0) +. (y >: #sorted) do. 0 else. y { sorted end.
)
combos =: dyad : 0 M.
  me =. at y
  next1 =. at (y + 1 + x)
  next2 =. at (y + 2 + x)
  if. y = ((#sorted) - 2) do.
    1
  elseif. ((next1 - me) > 3) do.
    0
  elseif. ((next2 - me) <: 3) do.
    (0 combos (y + 2 + x)) + ((x + 1) combos y)
  elseif. 1 do.
    0 combos (y + 1)
  end.
)

echo (0 combos 0)
exit''
