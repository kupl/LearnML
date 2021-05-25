(*problem 2*)
type nat= ZERO|SUCC of nat
let rec count:int->nat->int=fun n t->
match t with
|SUCC a-> count (n+1) a
|_-> n
let rec write: nat->int->nat=fun t n->
if n=0 then ZERO
else if n=1 then SUCC t
else write (SUCC (t)) (n-1)
let rec fastexpt:int->int->int=fun b n->
if b= 0 then 0
else  
  (if n=0 then 1
else if n mod 2=0 then fastexpt b (n/2) * fastexpt b (n/2)
else b*fastexpt b (n-1))
let natadd:nat->nat->nat=fun n1 n2->
write ZERO ((count 0 n1)+(count 0 n2))
let natmul:nat->nat->nat=fun n1 n2->
write ZERO ((count 0 n1)*(count 0 n2))
let natexp:nat->nat->nat=fun n1 n2->
write ZERO (fastexpt (count 0 n1) (count 0 n2))