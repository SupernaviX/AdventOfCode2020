readfile =: 1!:1
lines =: LF splitstring readfile <'input'
numbers =: 0 ". / "1 > lines                  NB. prase input to string
numbers =: numbers, 0, 3 + >./ numbers        NB. add 3+max and 0 to numbers
sorted =: /:~numbers
deltas =: (}.sorted) - }:sorted               NB. head of list plus (beheaded list) - (curtailed list) 
answer =: (+/deltas=1) * (+/deltas=3)         NB. count of 1s times count of 3s
echo answer
NB. exit''