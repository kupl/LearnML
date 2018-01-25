exception Problem
(*problem2*)

let rec smallest_divisor_sub : int->int->int
=fun a b->
if((b*b)>a) then a
else if a<2 then raise Problem
else if(a mod b=0) then b
else smallest_divisor_sub a (b+1)
let smallest_divisor : int->int 
= fun a->
smallest_divisor_sub a 2