(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
  | [] -> raise (Failure "The List is Empty")
  | hd::tl -> if tl = [] then hd
              else let maxcom x y = if x>y then x else y
                   in maxcom hd (max tl)

let rec min : int list -> int
= fun lst -> match lst with
  | [] -> raise (Failure "The List is Empty")
  | hd::tl -> if tl = [] then hd
              else let mincom x y = if x > y then y else x
                   in mincom hd (min tl)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
  match lst with
  | [] -> []
  | hd::tl -> if pred hd  then hd::(filter pred tl)
              else filter pred tl

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
  | Empty -> false
  | Node (x, Empty, Empty) -> if x == n then true
                              else false
  | Node (x, b1, b2) -> if x != n then mem n b1 || mem n b2
                        else true

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO -> n2
  | SUCC n -> natadd n (SUCC n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO -> ZERO
  | SUCC n -> natadd n2 (natmul n n2)

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
  | Not g -> if eval g then false else true
  | AndAlso (g, h) -> if eval g && eval h then true else false
  | OrElse (g, h) -> if eval g==false && eval h==false then false else true
  | Imply (g, h) -> if eval g && eval h == false then false else true
  | Equal (exp1, exp2) -> 
    let rec eval_exp e = match e with
      | Num i -> i
      | Plus (i1, i2) -> eval_exp i1 + eval_exp i2
      | Minus (i1, i2) -> eval_exp i1 - eval_exp i2 
    in if eval_exp exp1 == eval_exp exp2 then true else false
