(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> if List.length lst = 1 then List.hd lst else
  match lst with 
  | [] -> 0 
  | hd::tl -> if hd > max tl then hd else max tl
  ;;

let rec min : int list -> int
= fun lst -> if List.length lst = 1 then List.hd lst else
  match lst with 
  | [] -> 0 
  | hd::tl -> if hd < min tl then hd else min tl
  ;;
(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
	match lst with 
	| [] -> []
	| hd::tl -> if pred hd = true then hd::filter pred tl else filter pred tl;; (* TODO *)

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
match tree with 
	| Empty -> false
	| Node (num, left, right) -> if num = n then true else mem n left || mem n right;;(* TODO *)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
	| ZERO -> (match n2 with
		| ZERO -> ZERO
		| SUCC (succ_n2) -> SUCC (natadd n1 succ_n2)
		)
	| SUCC (succ_n1) -> SUCC (natadd succ_n1 n2);;(* TODO *)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
	| ZERO -> ZERO
	| SUCC (succ_n1) ->
		(match n2 with
		| ZERO -> ZERO
		| SUCC (succ_n2) -> natadd n1 (natmul n1 succ_n2)
		);; (* TODO *)

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
= fun f -> match f with
	| True -> true
	| False -> false
	| Not (for_val) -> not (eval for_val)
	| AndAlso (left, right) -> eval left && eval right
	| OrElse (left, right) -> eval left || eval right
	| Imply (left, right) -> if eval left = true then eval right == true else true
	| Equal (left, right) -> 
	let rec eval_exp : exp -> int = fun x -> match x with
	| Num (num_value) -> num_value
	| Plus (left, right) -> eval_exp left + eval_exp right
	| Minus (left, right) -> eval_exp left - eval_exp right
	in eval_exp left == eval_exp right (* TODO *)
;;
