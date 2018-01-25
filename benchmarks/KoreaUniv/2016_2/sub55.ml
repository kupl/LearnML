(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | x::[] -> x
  | x::tl -> let y = max tl in
                if y > x then y else x;;
(* TODO *)

let rec min : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | x::[] -> x
  | x::tl -> let y = min tl in
                if y < x then y else x;;
(* TODO *)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
    match lst with
    | [] -> []
    | hd::tl -> if pred hd
                    then hd :: filter pred tl
                    else filter pred tl;;
(* TODO *)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f a);; 
(* TODO *)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
  | Empty -> false
  | Node (x, left, right) -> n = x || (n < x && mem n left) || ( n > x && mem n right);;
(* TODO *)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
    | ZERO -> n2
    | SUCC(k) -> SUCC(natadd k n2);;
(* TODO *)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
    | ZERO -> ZERO
    | SUCC(k) -> natadd n2 (natmul k n2);;
(* TODO *)

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

