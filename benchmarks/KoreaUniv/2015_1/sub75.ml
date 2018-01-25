(* Helper functions *)
let rec map f l =
    match l with
    | [] -> []
    | head::tail -> (f head)::(map f tail);;

let rec fold f l acc =
    match l with
    | [] -> acc
    | head::tail -> f head (fold f tail acc);;

let (--) start halt = 
    let rec aux n acc =
        if n < start then acc else aux (n - 1) (n::acc)
    in aux halt [];;

(* Problem 1 *)
let rec pascal: int * int -> int
=fun (row,col) -> (* row, col *)
    match (row,col) with
        | _,0 -> 1
        | row,col when row = col -> 1
        | row,col -> pascal(row - 1,col) + pascal(row - 1,col - 1);;


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
    fold (+) (map f (a--b)) 0;;


(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
    | [e] -> e
    | h::t when h >= max t -> h
    | h::t -> max t
    | _ -> 0;;


let rec min : int list -> int
=fun l -> match l with
    | [e] -> e
    | h::t when h <= max t -> h
    | h::t -> min t
    | _ -> 0;;

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
=fun f -> match f with
    | True -> true
    | False -> false
    | Neg prop -> not (eval prop)
    | Or (prop1, prop2) -> eval prop1 || eval prop2
    | And (prop1, prop2) -> eval prop1 && eval prop2
    | Imply (prop1, prop2) -> not (eval prop1) || eval prop2
    | Equiv (prop1, prop2) -> eval prop1 = eval prop2

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec i2nat : int -> nat
=fun i -> match i with
    | 0 -> ZERO
    | n -> SUCC (i2nat (n-1))

let rec nat2i : nat -> int
=fun n -> match n with
    | ZERO -> 0
    | SUCC rem -> 1 + nat2i rem

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n2 with
    | ZERO -> n1
    | SUCC n -> SUCC (natadd n1 n);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> i2nat ((nat2i n1) * (nat2i n2));;

(*
print_string("Pascal Test\n");;
if pascal (0,0) != 1 then print_string("fail");;
if pascal (1,0) != 1 then print_string("fail");;
if pascal (1,1) != 1 then print_string("fail");;
if pascal (2,1) != 2 then print_string("fail");;
if pascal (4,2) != 6 then print_string("fail");;
print_string("----\n\n");;

print_string("Sigma Test\n");;
if sigma (fun x -> x) 1 10 != 55 then print_string("fail");;
if sigma (fun x -> x*x) 1 7 != 140 then print_string("fail");;
print_string("----\n\n");;

print_string("Max, Min Test\n");;
if max [1;3;5;2] != 5 then print_string("fail");;
if min [1;3;2] != 1 then print_string("fail");;
print_string("----\n\n");;

print_string("Prop Test\n");;
if eval False != false then print_string("fail");;
if eval True != true then print_string("fail");;
if eval (Neg False) != true then print_string("fail");;
if eval (Or (True, True)) != true then print_string("fail");;
if eval (Or (True, False)) != true then print_string("fail");;
if eval (Or (False, False)) != false then print_string("fail");;
if eval (And (True, True)) != true then print_string("fail");;
if eval (And (True, False)) != false then print_string("fail");;
if eval (And (False, False)) != false then print_string("fail");;
if eval (Imply (True, True)) != true then print_string("fail");;
if eval (Imply (True, False)) != false then print_string("fail");;
if eval (Imply (False, False)) != true then print_string("fail");;
if eval (Equiv (True, True)) != true then print_string("fail");;
if eval (Equiv (True, False)) != false then print_string("fail");;
if eval (Equiv (False, False)) != true then print_string("fail");;
if eval (Or (False, And (True, Imply ((Neg (Equiv (False, False))), False)))) != true then print_string("fail!!");;
print_string("----\n\n");;
*)
