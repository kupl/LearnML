(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int= fun lst ->
match lst with
|[] -> failwith "None"
|h::t ->  
let rec helper (seen,rest) =
match rest with 
|[] -> seen
|h'::t' -> let seen' =
if h' > seen then h' else seen in let rest' = t' 
in helper (seen',rest') in helper (h,t) 

let rec min : int list -> int= fun lst ->
match lst with
|[] -> failwith "None"
|h::t ->  
let rec helper (seen,rest) =
match rest with 
|[] -> seen
|h'::t' -> let seen' =
if h' < seen then h' else seen in let rest' = t' 
in helper (seen',rest') in helper (h,t) 
(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
match lst with
| [] -> []
| hd::lst' -> if pred hd 
	      then hd :: (filter pred lst') 
	      else filter pred lst'
(*********************)
(*     Problem 3     *)
(*********************)
let rec double f x = f( f x);;(* TODO *)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match tree with
|Empty-> false
|Node(m, left, right) ->
if n = m then true else
if n < m then mem n left else mem n right;; 

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
match n1 with
| ZERO -> n2
| SUCC m -> natadd m (SUCC n2);;

let rec natmul (n1 : nat) (n2:nat) : nat =
match n1 with
ZERO -> ZERO
| SUCC m -> natadd n2 (natmul m n2);;

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
= fun f -> true (* TODO *)
