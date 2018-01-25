(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 0

let rec max lst = let rec fold f l a = 
                        match l with
                        | [] -> a
                        | hd::tl -> f hd (fold f tl a)
in fold (fun x y ->if(x > y) then x else y) lst 0;;

let rec min : int list -> int
= fun lst -> 0

let rec min lst = let rec fold f l a =
                        match l with
                        | [] -> a
                        | hd::tl -> f hd (fold f tl a)
in fold (fun x y -> if(x < y) then x else y) lst 0;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = []

let rec filter pred lst =
                         match lst with
                         | [] -> []
                         | hd::tl -> if pred hd then hd::(filter pred tl)
                                     else (filter pred tl);;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = a

let rec double f (a) = f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> true

let rec mem n tree = 
                    match tree with
                    |Empty -> false
                    |Node (i, b1, b2) -> if i=n then true else mem n b1 || mem n b2;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> ZERO

let rec natadd n1 n2 = 
               match n1 with
               |ZERO -> n2
               |SUCC (n) -> SUCC (natadd n n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO

let rec natmul n1 n2 =
               match n1 with
               |ZERO -> ZERO
               |SUCC (n) -> natadd n2 (natmul n n2);;

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
= fun f -> true

let rec eval f = 

let rec expin f = 
               match f with
               | Num (n) -> n
               | Plus (e1, e2) -> expin e1 + expin e2
               | Minus (e1, e2) -> expin e1 - expin e2

          in   match f with
               | True -> true
               | False -> false
               | Not (f1) -> if f1 = True then false else true
               | AndAlso (f1, f2) -> if (f1 = True && f2 = True) then true else false
               | OrElse (f1, f2) -> if (f1 = True || f2 = True) then true else false
               | Imply (f1, f2) -> if (f1 = True && f2 = False) then false else true
               | Equal (e1, e2) -> if expin (e1) = expin (e2) then true else false

