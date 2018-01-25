(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> match n with
| 0 -> 1
| 1 -> b
| 2 -> b*b
| _ -> if (n mod 2 = 0) then (fastexpt b (n/2))*(fastexpt b (n/2))
  else b * (fastexpt b (n-1));;

(* problem 2*)
let rec inner n i = if i>(n/2) then n else
if (n mod i = 0) then i else inner n (i+1);;
let smallest_divisor : int -> int
= fun n -> inner n 2;;

(* problem 3*)
let compose f g = fun x ->f(g(x));;
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n = 0 then fun x->x
else if n = 1 then f
else compose f (iter((n-1),f));;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f(a) else f(a)*product f (a+1) b;;

(* problem 5*)

let dfact : int -> int
= fun n -> if n mod 2 = 0 then product (fun x -> 2*x) 1 (n/2)
  else product (fun x->2*x -1) 1 ((n+1)/2);;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with
| [] -> []
| h::t -> if n = 0 then l else if n = 1 then t else drop t (n-1);;

(* problem 7*)
let rec one lst =
match lst with
| [] -> []
| [(x,_)] -> [x]
|(hx,_)::tl -> [hx]@(one tl);;

let rec two lst = 
match lst with
|[] -> []
|[(_,y)] -> [y]
|(_,hy)::tl -> [hy]@(two tl);;
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (one lst, two lst);;

(* problem 8*)
