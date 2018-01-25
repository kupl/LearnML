(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> match lst with
	 []     -> 0
  |[h]    -> h
  |h::t   -> if h > max t then h else max t;;

let rec min : int list -> int
= fun lst -> match lst with
   []     -> 0
  |[h]    -> h
  |h::t   -> if h < min t then h else min t;;
(*********************)
(* Usually fold function uses exception case at the first of the recursive formula, so we should be treat some cases -for example, min fuction using List.fold_left and 0 as exception case always print 0 when the actual minimum value of the list is larger than 0 -. so I transform the basic code of fold function to operate as intended. *)
(*********************)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
  match lst with
   []   -> []
  |h::t -> if pred h then h::filter pred t
	  	  	 else filter pred t;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
	let fa = f a in f fa;;
(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
    Node (m, a, b) -> n = m || mem n a || mem n b
  | Empty          -> false ;; 
(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> let succ_n2 = SUCC (n2) in
    match n1 with
    ZERO     -> n2
  | SUCC (a) -> natadd a succ_n2;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let nat_match = (n1, n2) in
  match nat_match with
    (_, ZERO)
  | (ZERO, _)      -> ZERO
  | (SUCC (a) , b) -> natadd n2 (natmul a n2) ;;

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

let rec exp_eval : exp -> int
= fun e -> match e with
    Num a       -> a
  | Plus (a, b) -> exp_eval a + exp_eval b
  | Minus(a, b) -> exp_eval a - exp_eval b;;  

let rec eval : formula -> bool
= fun f ->  match f with
    True  -> true
  | False -> false 
  | Not a -> if eval a then false else true
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b)  -> eval a || eval b
  | Imply (a, b)   ->
      if eval a = true && eval b = false then false
      else true
  | Equal (a, b)   ->
      if exp_eval a = exp_eval b then true
      else false;; 
