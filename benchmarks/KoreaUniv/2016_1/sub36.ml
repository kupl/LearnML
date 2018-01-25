let rec fib n =
 if n> 2 then fib (n-1)+fib(n-2)
 else 1;;

let prime n =
 if n < 2 then false
 else if n = 2 then true
 else if n mod 2 = 0 then false
 else
  let sr = float_of_int n |> sqrt |> int_of_float in
  let rec check i =
   if i> sr then true
   else if n mod i = 0 then false
   else check (i+1)
  in
  check 3
;;

(* Problem 2 *)
let rec pascal (n,k)=
 if 0< k< n then pascal (n-1,k)+pascal (n-1,k-1)
 else pascal (n,k)=1;;

(* Problem 3 *)

(* Problem 4 *)
