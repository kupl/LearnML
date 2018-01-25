(* Problem 1 *) 
exception Don't_insert_minus
let rec fib x = 
if x<0 then raise Don't_insert_minus
else(
match x with
0->0
|1->1
|_->fib(x-1) + fib(x-2)
);;

(* Problem 2 *)
exception Don't_insert_y_which_bigger_than_x
let rec pascal (x, y) =
if x<0||y<0 then raise Don't_insert_minus
else if x<y then raise Don't_insert_y_which_bigger_than_x
else if x==y then 1 
else if y==0 then 1
else pascal (x-1,y-1) + pascal (x-1,y)
;;

(* Problem 3 *)
exception Don't_insert_zero
let rec prime1 n m = if m==1 then true
 else ((n mod m)!=0) &&(prime1 n (m-1));;
let prime n =
 if n<0 then raise Don't_insert_minus
 else if n == 0 then raise Don't_insert_zero
 else if n == 1 then false
 else let m = int_of_float(floor(sqrt(float_of_int n)))
 in prime1 n m
;;

(* Problem 4 *) 
exception Don't_insert_a_which_bigger_than_b
let rec sigma funx a b =
 if a<0||b<0 then raise Don't_insert_minus
 else if a>b then raise Don't_insert_a_which_bigger_than_b
 else if a<b then (funx a) + sigma funx (a+1) b
 else funx b
 ;;
