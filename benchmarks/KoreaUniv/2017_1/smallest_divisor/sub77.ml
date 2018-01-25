(*2*)
let rec odd_divisor n t = if n mod t = 0 then t else odd_divisor n (t+1);;
let smallest_divisor : int->int = fun n ->
if n mod 2=0 then 2 else odd_divisor n 2;;
