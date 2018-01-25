let rec fold f l a =
	match l with
	[] -> a
	| hd::tl -> f hd (fold f tl a);;


let rec fold_for_Pro1 f l =
	match l with
	[] -> raise (Failure "The array is empty")
	| hd::[] -> hd
	| hd::tl -> f hd (fold_for_Pro1 f tl);;

(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> fold_for_Pro1 (fun x y -> if (x>y) then x else y) lst;;

let rec min : int list -> int
= fun lst -> fold_for_Pro1 (fun x y -> if (x>y) then y else x) lst;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	fold (fun x y -> if (pred x) then x::y else y) lst [];;



(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =  f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree;;

let rec mem : int -> btree -> bool
= fun n tree -> 
	match tree with
	| Empty -> false
	| Node (a, b, c) -> if (a = n) then true else if (mem n b) then true else if (mem n c) then true else false;;


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat;;

let rec tra_to_int : nat -> int
= fun n ->
match n with 
| ZERO -> 0
| SUCC ZERO -> 1
| SUCC (a) -> tra_to_int a + 1;;

let rec tra_to_nat : int -> nat
= fun n ->
match n with
| 0 -> ZERO
| 1 -> SUCC ZERO
| a -> SUCC (tra_to_nat (a-1));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
tra_to_nat (tra_to_int n1 + tra_to_int n2);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
tra_to_nat (tra_to_int n1 * tra_to_int n2);;


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
	| Minus of exp * exp;;

let rec tra n = 
  match n with
    | Num a -> a
    | Plus (a, b) -> tra a + tra b
    | Minus (a, b) -> tra a - tra b;;

let rec eval : formula -> bool
= fun f ->
match f with
	| True -> true
	| False -> false
	| Not a -> not(eval a)
	| AndAlso (a, b) -> (eval a) && (eval b)
	| OrElse (a, b) -> (eval a) || (eval b)
	| Imply (a, b) -> not (eval a) || (eval b)
	| Equal (a, b) -> if (tra a) = (tra b) then true else false;;

