(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> (* TODO *)
match lst with
[] -> raise (Failure "Must Not Be Empty!")
|[one] -> one
|hd::tl -> if ( hd > (max tl) ) then hd else (max tl)

let rec min : int list -> int
= fun lst -> (* TODO *)
match lst with
[] -> raise (Failure "Must Not Be Empty!")
|[one] -> one
|hd::tl -> if ( hd < (min tl) ) then hd else (min tl)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = (* TODO *)
match lst with
[] -> []
|hd::tl -> if ( pred hd ) then hd::( filter pred tl )
else ( filter pred tl )

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =  (* TODO *)
f(f(a))

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> (* TODO *)
match tree with
Empty -> false
|Node (a, b, c) -> 
if (a=n) then true
  else if (mem n b) then true
    else if (mem n c) then true else false

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
match n1 with
ZERO -> n2
|SUCC(sth) -> SUCC( natadd sth n2 )

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
match n1 with
ZERO -> ZERO
|SUCC(sth) -> natadd n2 (natmul sth n2)

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
= fun f -> (* TODO *)
match f with
True -> true
|False -> false
|Not x -> not (eval x)
|AndAlso (x,y) -> (eval x) && (eval y)
|OrElse (x,y) -> (eval x) || (eval y)
|Imply (x,y) -> if ((eval x)=false) then true else (eval y)
|Equal (x,y) ->  
  let rec calcul = fun expr ->
    match expr with
     Num a -> a
    |Plus(m, n) -> (calcul m) + (calcul n)
    |Minus(m, n) -> (calcul m) - (calcul n)
  in
   if ((calcul x)=(calcul y)) then true else false
