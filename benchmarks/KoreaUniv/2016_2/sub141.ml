(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun list -> 
  match list with
   | [] -> raise (Failure "input list is empty")
   | _ -> List.fold_left (fun a b ->
          if a > b then a else b) (List.hd list) list

let rec min : int list -> int
= fun list ->
   match list with
   | [] -> raise (Failure "input list is empty")
   | _ -> List.fold_left (fun a b ->
          if a < b then a else b) (List.hd list) list

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter : ('a -> bool) -> 'a list -> 'a list
= fun pred list -> []


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a : ('a -> 'a) -> 'a -> 'a
= fun f a -> f ( f a )

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
  match tree with
   | Empty -> false
   | Node (z, left, right) ->
    if n = z then true else
    if n < z then mem n left else mem n right

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

