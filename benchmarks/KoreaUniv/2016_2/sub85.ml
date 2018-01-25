(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
match l with
|[] -> a
|hd::tl -> f hd (fold f tl a);;

let rec max : int list -> int
= fun lst -> fold (fun a b -> if (a > b) then a else b) lst 0;;

let rec min : int list -> int
= fun lst -> fold (fun a b -> if (a < b) then a else b) lst 0;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = fold (fun a b -> if (pred a) then a::b else b) lst [];;

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
= fun n tree ->
match tree with 
|Empty->false
|Node(x,y,z)->
if(x=n) then true 
else if (mem n y) then true 
else if (mem n z) then true 
else false;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec chg_nat : nat->int
=fun n-> match n with
|ZERO -> 0
|SUCC ZERO -> 1
|SUCC(a) -> chg_nat a+1;;

let rec chg_int : int->nat
=fun n -> match n with
|0->ZERO
|1->SUCC ZERO
|a->SUCC (chg_int(a-1));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> chg_int(chg_nat n1 + chg_nat n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> chg_int(chg_nat n1 * chg_nat n2);;

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

let rec chg n =
match n with
|Num x -> x
|Plus(x,y) -> chg x + chg y
|Minus(x,y) -> chg x - chg y;;

let rec eval : formula -> bool
= fun f -> match f with
|True->true
|False->false
|Not x -> not(eval x)
|AndAlso(x, y) -> (eval x) && (eval y)
|OrElse(x, y) -> (eval x) || (eval y)
|Imply(x, y) -> not (eval x) || (eval y)
|Equal(x, y) -> if (chg x)=(chg y) then true else false;;
