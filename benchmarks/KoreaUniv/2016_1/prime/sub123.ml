(* Problem 1 *) 
exception Dont_insert_minus
exception Dont_insert_zero
let rec prime1 n m = if m==1 then true
 else ((n mod m)!=0) &&(prime1 n (m-1));;
let prime n =
 if n<0 then raise Dont_insert_minus
 else if n == 0 then raise Dont_insert_zero
 else if n == 1 then false
 else let m = int_of_float(floor(sqrt(float_of_int n)))
 in prime1 n m
;;
