let rec prime : int -> bool
= fun n ->
let rec divisible a =
(n>a)&&(n mod a = 0 || divisible (a+1)) in
if n=1 then false
else if n<1 then false
else if n=2 then true
else if divisible(2) then false
else true
