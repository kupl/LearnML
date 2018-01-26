(* Problem 1 *) 
exception Dont_insert_minus
let rec fib x = 
if x<0 then raise Dont_insert_minus
else(
match x with
0->0
|1->1
|_->fib(x-1) + fib(x-2)
);;
