(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->  match lst with 
 | []->0
 | h::t -> let rec fun2 : int list -> int -> int
 = fun lst1 a -> match lst1 with 
 | []-> a
 | hd::tl -> if hd>a then fun2 tl hd else fun2 tl a in fun2 t h
 
let rec min : int list -> int
= fun lst -> match lst with 
 | []->0
 | h::t -> let rec fun2 : int list -> int -> int
 = fun lst1 a -> match lst1 with 
 | []-> a
 | hd::tl -> if hd<a then fun2 tl hd else fun2 tl a in fun2 t h

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
 | [] -> []
 | hd::tl -> (if pred hd = true then [hd] else []) @ (filter pred tl)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f (a))

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
 | Node(i,b1,b2) ->
  if i=n then true else mem n b1 || mem n b2

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
 | ZERO -> n2
 | SUCC(n) -> SUCC(natadd n n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let rec fun2 a b c = 
match a with 
 | ZERO -> c
 | SUCC(aa) -> fun2 aa b (natadd b c) in fun2 n1 n2 ZERO

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
 | Not (a) -> if eval a = true then true else false
 | AndAlso(a,b) -> if eval(a)=true && eval(b) = true then true else false
 | OrElse (a,b)-> if eval(a)=false && eval(b) = false then false else true
 | Imply (a,b) -> if eval(a)=true && eval(b) = false then false else true
 | Equal (a,b) -> let rec eq : exp-> int =  fun e ->
 match e with 
 | Num (a)->a
 | Plus(a,b)-> eq(a)+eq(b)
 | Minus (a,b)-> eq(a) - eq(b) in
 if(eq)a = eq (b) then true else false


