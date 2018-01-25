(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
  = fun lst ->
  let rec maxx : int -> int -> int
  	= fun a b ->
  	if a > b then a else b in
  match lst with
  |m::[] -> m
  |m::n -> maxx m (max n);;


let rec min : int list -> int
	= fun lst ->
	let rec minn : int -> int -> int
		= fun a b ->
		if a > b then b else a in
	match lst with
	|m::[] -> m
	|m::n -> minn m (min n);;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
  match lst with
  |[] -> []
  |a::[] -> if pred a = true then [a] else []
  |a::b -> filter pred [a] @ filter pred b;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a=
  f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
        | Empty
        | Node of int * btree * btree

  let rec mem : int -> btree -> bool
  = fun n tree ->
  match tree with
  |Empty -> false
  |Node(a, t1, t2) ->
  begin if a = n then true
  else if mem n t1 = true then true
  else if mem n t2 = true then true
  else false
  end;;

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
|SUCC r -> natadd r (SUCC n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC r ->natadd (natmul r n2) n2;;


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
= fun f ->
let rec change : exp -> int
 = fun ex ->
match ex with
|Num a -> a
|Plus(a, b) -> (change a) + (change b)
|Minus(a, b) -> (change a) - (change b)
in
match f with
|True -> true
|False -> false
|Not m -> if eval m = true then false else true
|AndAlso(m, n) -> if eval m = true && eval n = true then true else false
|OrElse(m, n) -> if  eval m = true || eval n = true then true else false
|Imply(m, n) -> if eval m = true  && eval n = false then false
else true
|Equal(a, b) -> if (change a) = (change b) then true else false
;;
