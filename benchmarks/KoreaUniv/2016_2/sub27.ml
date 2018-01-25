(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l =
match l with
[a] -> a 
|[a; b] -> f a b
|hd::tl -> fold f (hd::fold f tl::[]);; 

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x>y then x else y) lst;; (* TODO *)

let rec min : int list -> int
= fun lst -> fold (fun x y -> if x<y then x else y) lst;; (* TODO *)
(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =  
match lst with
[] -> []
| hd::tl -> (if pred hd then [hd] else []) @ filter pred tl ;;  (* TODO *)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);; (* TODO *)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match  tree with
Empty -> false
|Node (a , ltree , rtree) -> if a=n then true else mem n ltree || mem n rtree;; (* TODO *)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with
ZERO ->  n2
| SUCC ZERO -> SUCC n2 
|SUCC (SUCC n) -> SUCC (SUCC (natadd n n2));;(* TODO *)
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with
ZERO -> ZERO
| SUCC ZERO -> n2
| SUCC n -> natadd n2 (natmul n n2);;  (* TODO *)

(*********************)
(*     Problem 6     *)
(*********************)
type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec eval : formula -> bool
= fun f -> 
match f with
True -> true
|False -> false
|Not a -> if a = True then false else true
|AndAlso (a, b)-> if a = True && b = True then true else false
|OrElse (a, b) -> if a = True || b = True then true else false
|Imply (a, b) -> if (Not a) = True ||  b = True then true else false
|Equal (a, b) -> let rec evalexp e 
= match e with
Num a -> a
|Plus (a, b) -> evalexp a + evalexp b
|Minus (a, b) -> evalexp a - evalexp b
in if evalexp a = evalexp b then true else false;;
 (* TODO *)

