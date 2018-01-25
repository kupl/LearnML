(* problem 1*)
let rec fastexpt : int -> int -> int = fun b n ->
  if n=2 then b * b
  else if (n mod 2 = 1) then b * (fastexpt b (n-1))
  else (fastexpt b (n/2)) * (fastexpt b (n/2));;

(* problem 2*)

let smallest_divisor : int -> int = fun n ->
  let a = int_of_float(sqrt(float_of_int n)) in
  let rec f i = 
  	if a < i then n
  	else if n mod i = 0 then i
  	else f (i+1) in
  f 2;;
  

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int) = fun (n,f) -> 
  if n = 0 then (fun (x : int) -> x)
  else (fun (x : int) -> f (iter((n-1), f) x));;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int = fun f a b -> 
  if a = b then f a
  else (f b) * (product f a (b-1));;

(* problem 5*)

let rec dfact : int -> int
= fun n -> 
  if n = 1 then 1
  else if n = 2 then 2
  else n * dfact (n-2) ;; 

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
  match l with
  |[] -> []
  | hd::tl -> if n = 1 then tl else drop tl (n-1);;

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
  match lst with
  | [] -> ([],[])
  | (x,y)::tl -> let l1, l2 = unzip tl in x::l1, y::l2;;

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
 if amount = 0 then 1
 else if amount < 0 then 0
 else
 match coins with
 |[] -> 0
 |hd::tl -> if (hd > amount) then (change tl amount) else (change tl amount) + (change coins (amount - hd));;
 
