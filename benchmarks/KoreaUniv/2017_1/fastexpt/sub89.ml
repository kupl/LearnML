(*problem1*)
let rec fastexpt : int->int->int = fun b n ->
  if n=1 then b
  else
  if n mod 2 = 0 then fastexpt (b*b) (n/2)
  else b*(fastexpt b (n-1));;
