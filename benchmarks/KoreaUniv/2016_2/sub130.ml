(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
        match l with
        | [] -> a
        | hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> match lst with
        | [] -> min_int
        | hd::tl -> let tmp = max tl in
        if tmp > hd then tmp else hd;;

let rec min : int list -> int
= fun lst -> match lst with
        | [] -> max_int
        | hd::tl -> let tmp = max tl in
        if tmp < hd then tmp else hd;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
        | [] -> []
        | hd::tl -> if pred hd then hd :: filter pred tl
        else filter pred tl;;

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
= fun n tree -> match tree with
        | Empty -> false
        | Node (a, b, c) -> if a = n then true
        else false || mem n b || mem n c;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
        | ZERO -> n2
        | SUCC(n3) -> SUCC(natadd n3 n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
        | ZERO -> ZERO
        | SUCC(n3) -> natadd n2 (natmul n3 n2);;

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

let rec evalExp: exp -> int
= fun ex -> match ex with
        | Num(a) -> a
        | Plus(a, b) -> evalExp(a) + evalExp(b)
        | Minus(a, b) -> evalExp(a) - evalExp(b);;
let rec eval : formula -> bool
= fun f -> match f with
        | True -> true
        | False -> false
        | Not(a) -> not(eval a)
        | AndAlso(a, b) -> eval(a) && eval(b)
        | OrElse(a, b) -> eval(a) || eval(b)
        | Imply(a, b) -> let c = eval(a) in
                        let d = eval(b) in
                        if c && not d then false else true
        | Equal(a, b) -> if evalExp(a) = evalExp(b) then true else false;;
