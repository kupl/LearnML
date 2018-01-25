(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> 
 match n with
 | 1 -> b
 | 2 -> b * b
 |_ ->  if n mod 2 = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
 else b * (fastexpt b (n-1))


(* problem 2*)
let rec smallest_divisor : int -> int
= fun n -> 
 match n with 1 -> raise (Failure "Please write a number greater than 1")
 |_ -> let rec aux n i = if i  > (n/2) then n else
 if (n mod i = 0) then i else aux n (i+1) in aux n 2


(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) x ->
 match n with
 | 0 -> x
 |_ -> f(iter(n-1, f) x ) 


(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
 if a = b then f(a)
 else f(b) * product f a (b-1) 


(* problem 5*)
let rec dfact : int -> int
= fun n -> 
 match n with
 | 1 -> 1
 | 2 -> 2
 |_ -> if n mod 2 = 1 then  n * dfact (n-2)
       else n * dfact (n-2) 


(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> 
 match l with
 | [] -> []
 | h::t -> if n=0 then h::t else drop t (n-1)


(* problem 7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
 match lst with
 | [] ->([],[])
 | (h1,h2)::t -> let (t1, t2) = unzip t in (h1::t1, h2::t2)

(* problem 8*)

