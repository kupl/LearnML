(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 0 


let rec max : int list -> int
= fun lst ->
 let rec rmax : int -> int list -> int
= fun m l ->
	match l with
		[] -> m
		| x::xs -> if m > x then rmax m xs else rmax x xs  
	in
		match lst with
		x::xs -> rmax x xs;;



let rec min : int list -> int
= fun lst -> 
	let rec rmin : int -> int list -> int
= fun m l ->
	match l with
		[] -> m
		| x::xs -> if x > m then rmin m xs else rmin x xs
	in
		match lst with
		x::xs -> rmin x xs;;

(*********************)
(*     Problem 2     *)
(*********************)

let rec filter : ('a -> bool) -> 'a list -> 'a list
= fun pred lst ->
let rec rfilter : 'a list -> 'a list
= fun l ->
match l with 
|[] -> []
|h::t -> if pred h = true then (h::rfilter t ) else rfilter t in
rfilter lst;;



(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;


(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match tree with

Empty -> false
|Node (a, l, r) -> if n = a then true
else mem n l|| mem n r;;


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)

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

