(*********************)
(*     Problem 1     *)
(*********************)

let rec fold f l a =
  match l with
  | [] -> a
  | hd::tl -> f hd (fold f tl a)
 




let biggest = min_int

let rec max : int list -> int
= fun lst ->
	fold (fun a b -> if a>b then a else b) lst biggest

let smallest = max_int

let rec min : int list -> int
= fun lst -> 
	fold (fun a b -> if a>b then b else a) lst smallest



(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
	match lst with
	| [] -> []
	| hd::tl -> if pred hd = true then (filter pred tl) @ [hd]
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
= fun n tree -> 
	match tree with
	| Empty -> false
	| Node(num, left, right) -> 
	if num <> n then (mem n left) || (mem n right)													else true



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
	| SUCC a -> SUCC (natadd a n2)




let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n1 with
	| ZERO -> ZERO
	| SUCC a -> natadd (natmul a n2) n2





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



let rec expformula : exp -> int
= fun exp ->
	match exp with
	| Num (exp) -> exp
	| Plus (exp1, exp2) -> (expformula exp1) + (expformula exp2)
	| Minus (exp1, exp2) -> (expformula exp1) - (expformula exp2)
  


let rec eval : formula -> bool
= fun f -> 
	match f with
	| True -> true
	| False -> false
	| Not fm -> not (eval fm)
	| AndAlso (fm1, fm2) -> (eval fm1) && (eval fm2)
	| OrElse (fm1, fm2) -> (eval fm1) || (eval fm2)
	| Imply (fm1, fm2) -> if eval fm1 = false then true
	 									else if eval fm2 = true then true
										else false
	| Equal (exp1, exp2) -> if expformula(exp1) = expformula(exp2) 
														then true
													else false



