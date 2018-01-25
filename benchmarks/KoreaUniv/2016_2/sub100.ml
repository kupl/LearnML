(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
  match l with
  | [] -> a
  | hd::tl -> f hd (fold f tl a)

let max lst 
  = fold (fun x y -> (if x>y then x else y)) lst 0

let min lst
  = fold (fun x y -> (if x<y then x else y)) lst 100000000

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
  match lst with
  | [] -> []
  | hd::tl -> if (pred hd) then (hd::(filter pred tl)) else (filter pred tl)

(*********************)
(*     Problem 3     *)
(*********************)
let double f a = f(f a)

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
  | Node(num, left, right) -> if num=n then true
                              else if mem n left then true 
                              else  mem n right

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> let rec f a =
              match a with
              | ZERO -> ZERO
              | SUCC (b) -> SUCC (f b)
            in f n2
  | SUCC (c) -> SUCC (natadd c n2)  

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC (c) -> (natadd n2 (natmul n2 c))

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
  | Not a -> if eval a then false else true
  | AndAlso (a, b) -> if eval a then eval b else false
  | OrElse (a, b) -> if eval a then true else eval b
  | Imply (a, b) -> if eval a=false then true else eval b
  | Equal (a, b) -> let rec cal : exp -> int
                      = fun g ->
                        match g with
                        | Num a ->  a
                        | Plus (a, b) -> cal a + cal b
                        | Minus (a, b) -> cal a - cal b
                        in if cal a=cal b then true else false
