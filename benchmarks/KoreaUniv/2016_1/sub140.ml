(* Problem 1 *)
let rec fib n =
 match n with
 0 -> 0
|1 -> 1
|_ -> fib(n-1)+fib(n-2);;

(* Problem 2 *)
let rec pascal (n1, n2) =
 if n2==0 then 1
 else if n1==n2 then 1
 else pascal(n1-1,n2-1)+pascal(n1-1,n2);;

(* Problem 3 *)
let rec prime n =
 let rec sub_prime (i,j)
  if j==1 then true
  else if (i mod j) == 0 then false
  else sub_prime(i,j-1) in
  sub_prime(n,int_of_float(sqrt(float_of_int(n))));;

(* Problem 4 *)
let rec sigma f a b =
 if a == b then f a
 else sigma f a (b-1) + f b;;