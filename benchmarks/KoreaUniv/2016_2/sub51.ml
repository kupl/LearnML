(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l =
  match l with
  | [a] -> a
  | hd::tl -> f hd (fold f tl)

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x > y then x else y) lst

let rec min : int list -> int
= fun lst -> fold (fun x y -> if x < y then x else y) lst

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
  match lst with
  | [] -> []
  | h::t ->
    if pred h then h::(filter pred t)
    else filter pred t

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
  | Node (x, lchild, rchild) -> (x = n) || (mem n lchild) || (mem n rchild)

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
  | SUCC n -> natadd n (SUCC n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC n -> natadd n2 (natmul n n2)

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

let imply v =
  match v with
  | (true, x) -> x
  | (false, x) -> true

let rec calc e =
  match e with
  | Num n -> n
  | Plus (e1, e2) -> calc e1 + calc e2
  | Minus (e1, e2) -> calc e1 - calc e2

let rec eval : formula -> bool
= fun f ->
  match f with
  | True -> true
  | False -> false
  | Not fm -> not (eval fm)
  | AndAlso (fm1, fm2) -> (eval fm1) && (eval fm2)
  | OrElse (fm1, fm2) -> (eval fm2) || (eval fm2)
  | Imply (fm1, fm2) -> imply (eval fm1, eval fm2)
  | Equal (e1, e2) -> (calc e1) = (calc e2)
