(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
   match lst with
   | hd :: [] -> hd
   | hd :: tl -> big hd (max tl)
   and big a b =
      if a < b then b
      else a
let rec min : int list ->int
= fun lst ->
   match lst with
   | hd :: [] -> hd
   | hd :: tl -> small hd (min tl)
   and small a b = 
      if a < b then a
      else b


(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
   List.filter pred lst


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
  | Node (i, l, r) -> (n = i) or (mem n l) or (mem n r) or (false)


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
   | SUCC r -> SUCC (natadd r n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
   match n1 with
   | ZERO -> ZERO
   | SUCC r -> natadd n2 (natmul r n2)


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
match f with
	| True -> true
	| False -> false
	| Not a -> not (eval a)
	| AndAlso (a, b) -> (eval a) && (eval b)
	| OrElse (a, b) -> (eval a) || (eval b)
	| Imply (a, b) -> not (eval a) || (eval b)
	| Equal (a, b) -> ((evall a) = (evall b))
and evall : exp -> int
= fun f ->
match  f with
	| Num a -> a
	| Plus (a, b) -> (evall a) + (evall b)
	| Minus (a, b) -> (evall a) - (evall b)