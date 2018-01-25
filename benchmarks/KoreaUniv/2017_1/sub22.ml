(* problem 1*)

let rec fastexpt : int -> int -> int
 = fun b n ->
 if n = 0 then 1
 else if (n mod 2 = 0) then (fastexpt b (n/2))*(fastexpt b (n/2))
 else b * (fastexpt b (n-1));;

(* problem 2*)

let smallest_divisor: int -> int
= fun n ->
let rec aux n d=
if(n mod 2 =0) then 2
else if float_of_int d> sqrt(float_of_int n) then n
else if (n mod d = 0) then d
else aux n (d+2) in aux n 3;;


(* problem 3*)

let rec iter : int*(int->int)->(int->int)
 =fun(n, f) a ->
 if n=0 then a
 else if n=1 then f(a)
 else iter(n-1, f) (iter(1, f) a);;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
 = fun f a b ->
	if a=b then f(a)
	else product f a (b-1) * f(b);;

(* problem 5*)

let rec dfact : int -> int
 = fun n ->
 if (n=1 || n=2) then n
 else dfact (n-2) * n;;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
 = fun l n ->
 if n = 0 then l
 else if (List.length l = 0) then l
 else (drop(match l with a::b -> b)(n-1));;

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*TODO*)

(* problem 8*)

let rec change_get coins len amount
= if amount = 0 then 1
  else if amount<0 then 0
  else if len = 0 then 0
  else (change_get coins (len-1) amount)+(change_get coins len(amount-List.nth coins (len-1)));;

let change coins amount
= let len = List.length coins in
  change_get coins len amount;;
