(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> 
  if n = 1 then 2
  else if n mod 2 = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
  else b * (fastexpt b (n-1))


(* problem 2*)
let smallest_divisor : int -> int
= fun n ->
  let rec test a n =
    if n mod a = 0 then a
    else test (a+1) n in
    test 2 n


(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  let rec iter_inter (n,f) x =
  if n = 0 then x
  else if n = 1 then (f x) 
  else (f x) * (iter_inter ((n-1),f) x) in
  iter_inter (n,f)



(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b ->
  let rec product_inter = fun f a b ->
   if b = a then f a 
   else (f b) * product_inter f a (b-1) in
   product_inter f a b 


(* problem 5*)

let dfact : int -> int
= fun n -> 
  if n mod 2 = 0 then product (fun x->2*x) 1 (n/2)
  else product (fun x-> ((2*x)-1)) 1 ((n+1)/2)



(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
match l with
  [] -> []
  |h::t -> if n = 0 then h::t else (drop t (n-1))



(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
 match lst with
    |[] -> ([], [])
    |(head1,head2)::tail -> 
    let (headoftail1, headoftail2) = unzip tail in 
    (head1::headoftail1, head2::headoftail2)



(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> 
 if amount = 0 then 1
 else if ((amount < 0) || (coins = [])) then 0
 else 0

