(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if n = 1 then b 
  else if n mod 2 = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
  else b * (fastexpt b ((n-1)/2)) * (fastexpt b ((n-1)/2));;

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let m = 2 in
  let rec func n m =
  if m * m > n then n
  else (if n mod m = 0 then m
  else func n (m+1)) in func n m;;


(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> let compose f g = fun x -> f(g(x)) in
  if n = 0 then fun x -> x
  else if n = 1 then f
  else compose f (iter(n-1, f));;


(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a<b then (f a) * (product f (a+1) b)
  else f b;;

(* problem 5*)

let rec dfact : int -> int
= fun n -> if n mod 2 = 0 then product(fun x -> 2*x) 1 (n/2)
else (product (fun x -> x) 1 n) / (dfact (n-1)) ;;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> if n = 0 then l
  else match l with
  | [] -> []
  | hd::tl -> drop tl (n-1);;

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let fst (x,_) (list1, list2) = (x::list1, list2) in
  let snd (_,y) (list1, list2) = (list1, y::list2) in
  match lst with
  | [] -> ([],[])
  |hd::[] -> fst hd (snd hd ([],[]))
  |hd::tl -> fst hd (snd hd (unzip tl));;

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> if amount < 0 then 0
  else if amount = 0 then 1
  else match coins with
  | [] -> 0
  | hd::[] -> if amount mod hd = 0 then 1 else 0
  | hd::tl -> if amount < hd then 0
  else (change (hd::tl) (amount-hd)) + (change tl amount);;

