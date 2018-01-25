(* Problem 1 *)
let pascal : int * int -> int
=fun (x,y) ->
        let rec factorial n = if n <= 0 then 1
                                         else n * factorial (n - 1) in
        (factorial x) / ((factorial y) * (factorial (x - y)))

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a >= b then f a
                        else (f a) + sigma f (a + 1) b

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
        | [] -> raise (Failure "List is Empty")
        | hd :: [] -> hd
        | hd :: tl -> if hd > max tl    then hd
                                        else max tl

let rec min : int list -> int
=fun l -> match l with
        | [] -> raise (Failure "List is Empty")
        | hd :: [] -> hd
        | hd :: tl -> if hd > min tl    then min tl
                                        else hd

(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f ->
        let negb b : bool =
                if b then false else true in
        let orb ba bb : bool =
                if ba then true else bb in
        let andb ba bb : bool =
                if ba then bb else false in
        let implyb ba bb : bool =
                if ba then bb else true in
        let equivb ba bb : bool =
                if ba = bb then true else false in
        match f with
        | True -> true
        | False -> false
        | Neg a -> negb (eval a)
        | Or (a, b) -> orb (eval a) (eval b)
        | And (a, b) -> andb (eval a) (eval b)
        | Imply (a, b) -> implyb (eval a) (eval b)
        | Equiv (a, b) -> equivb (eval a) (eval b)

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
                | ZERO -> n2
                | SUCC (a) -> natadd a (SUCC (n2))

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->   let rec natmul_sub a b c =
                match a with
                | ZERO -> b
                | SUCC (a) -> natmul_sub a ( natadd b c ) c in
                natmul_sub n1 ZERO n2

