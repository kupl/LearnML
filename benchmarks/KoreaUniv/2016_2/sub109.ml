let rec fold f l a =
 match l with
  | [] -> a
  | hd::tl -> f hd (fold f tl a)

let greater a b =
	if a>=b then a
	else b
let less a b = 
	if a>=b then b
	else a


let rec max : int list -> int
= fun lst -> 
    match lst with
      | hd::tl -> fold greater tl hd

let rec min : int list -> int
= fun lst ->  
	match lst with
      | hd::tl -> fold less tl hd


(*********************)

(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] -> []
	| hd::tl -> if pred(hd) = true then  hd::(filter pred tl) 
				else (filter pred tl)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
f (f a)


(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> (* TODO *)
match tree with 
 | Empty -> false
 | Node (m, t1, t2) -> if m =n then true
 					   else mem n t1 || mem n t2

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with
 |ZERO -> n2
 |SUCC ZERO -> SUCC(n2)
 |SUCC (nat) -> let n3 = SUCC(n2) in natadd nat n3 

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
 match n1 with
 |ZERO -> ZERO
 |SUCC ZERO -> n2
 |SUCC (nat) -> let n3 = natmul nat n2 in natadd n3 n2 

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

let rec nexp : exp -> int
= fun e ->
	match e with
	|Num n -> n
	|Plus (e1, e2) -> nexp e1 + nexp e2
	|Minus (e1, e2) -> nexp e1 - nexp e2

let rec eval : formula -> bool
= fun f ->
  match f with 
  |True -> true
  |False -> false
  |Not(f1) -> if eval f1 = true then false else true
  |AndAlso(f1, f2) -> eval f1&& eval f2
  |OrElse(f1, f2) -> eval f1|| eval f2
  |Imply(f1, f2) -> if eval f1=true && eval f2=false then false else true
  |Equal(e1,e2) -> if nexp (e1) = nexp (e2) then true else false