(*problem 1*)

let rec fastexpt : int-> int-> int
= fun b n -> if n =0 then 1
  else if n  mod 2 =  0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
  else b * (fastexpt b (n-1))
;;
(*problem 2*)
let smallest_divisor : int-> int
=fun n -> let rec division :int->int->int 
  =fun n i ->  if n mod 2 =0 then 2 
   else if i>int_of_float ( sqrt( float(n))) then n 
   else if n  mod i = 0 then i
   else division n (i+1) in division n 3
;;
  
(*problem 3*)

let rec iter : int*(int->int)->(int->int)
= fun (n,f) -> 
if n=0 then fun x->x
else if n=1 then fun x-> f(x)
else fun x -> iter((n-1), f ) (f x)
;;
(*problem 4*)
let rec product : (int->int)->int->int ->int
=fun f a b ->
if a=b then f a
else f a *product f (a+1) b
;;
(*problem 5*)
let dfact : int-> int
= fun n ->
if n =0 then 1
else if n mod 2 =0 then product (fun x-> x*2) 1 (n/2)
else product (fun x-> (x*2)-1) 1 ((n+1)/2)
;;

(*problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n->
if n=0 then l
else match l with
  |[]->[]
  |hd::tl -> drop tl (n-1)
;;
(*problem 7*)
let rec  unzip : ('a *'b) list -> 'a list *'b list
= fun lst ->
match lst with
|[] -> ([],[])
|(x,y) ::tl -> let(l1,l2)=unzip tl in (x::l1, y::l2)
;;

(*problem 8 *)


let change : int list -> int -> int
= fun coins amount ->
let rec count coins amount idx=
if amount = 0 then 1
else if (amount < 0) || (idx >= List.length coins) then 0
else (count coins amount (idx+1)) + (count coins (amount - (List.nth coins idx)) idx) in count coins amount 0
