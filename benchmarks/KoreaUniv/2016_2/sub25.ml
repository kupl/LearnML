open Printf
(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
        match l with
        | [] -> a
        | hd::tl -> f hd (fold f tl a);;

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x > y then x else y) lst 0;;

printf("Problem 1 ----------------\n");;
let list = [0; 6; 2; 3] in
printf "max : %d\n" (max list);;


let rec min : int list -> int
= fun lst -> fold (fun x y -> if x < y then x else y) lst 0;;
(*********************)
(*     Problem 2     *)
(*********************)
let list = [0; 6; 2; 3] in
printf "min : %d\n" (min list);;
printf "\n";;

let rec filter pred lst = 
        match lst with
        | [] -> []
        | hd::tl -> if pred hd then hd::(filter pred tl) else (filter pred tl);;
        
printf("Problem 2 ----------------\n");;
printf("filtered : ");;
let list = [0; 6; 2; 3] in
let filtered = (filter (fun x -> x mod 2 = 0) list) in
List.iter (fun a -> printf "%d " a) filtered;;
printf "\n";;
printf "\n";;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

let inc x = x + 1;;
let mul x = x * 2;;

printf("Problem 3 ----------------\n");;
printf "(double inc) 1 : %d\n" ((double inc) 1);;
printf "((double double) inc) 0 : %d\n" (((double double) inc) 0);;
printf "((double (double double)) inc) 5 : %d\n" (((double (double double)) inc) 5);;
printf "(double mul) 1 : %d\n" ((double mul) 1);;
printf "(double double) mul 2 : %d\n" (((double double) mul) 2);;
printf "\n";;

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
        | Node(value, left, right) -> if value = n then true else (mem n left) || (mem n right);;

let t1 = Node (1, Empty, Empty);;
let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty));;

printf("Problem 4 ----------------\n");;
printf "%B\n" (mem 1 t1);;
printf "%B\n" (mem 4 t2);;
printf "%B\n" (mem 3 t2);;
printf "\n";;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
        match (n1, n2) with
        | (ZERO, ZERO) -> ZERO
        | (_, ZERO) -> n1
        | (ZERO, _) -> n2
        | (SUCC k, _) -> SUCC (natadd k n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
        match (n1, n2) with
        | (_, ZERO) -> ZERO
        | (ZERO, _) -> ZERO
        | (SUCC k, _) -> natadd n2 (natmul k n2);;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
let four = SUCC (SUCC (SUCC (SUCC ZERO)));;

printf("Problem 5 ----------------\n");;
natadd two four;;
natadd ZERO three;;
natadd two ZERO;;
natmul two three;;
natmul four three;;
natmul two ZERO;;
printf("\n");;

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

let rec exp_to_int : exp -> int
= fun e ->
        match e with
        | Num n -> n
        | Plus (e1, e2) -> (exp_to_int e1)  + (exp_to_int e2) 
        | Minus (e1, e2) -> (exp_to_int e1) - (exp_to_int e2);;

exp_to_int (Num 3);;
exp_to_int (Plus (Num 3, Num 4));;
exp_to_int (Minus (Num 3, Num 4));;


let rec eval : formula -> bool
= fun f ->
        match f with
        | True -> true
        | False -> false
        | Not f1 -> if (eval f1) then false else true
        | AndAlso (f1, f2) -> if (eval f1) then (eval f2) else false
        | OrElse (f1, f2) -> if (eval f1) then true else (eval f2)
        | Imply (f1, f2) -> eval (OrElse (Not f1, f2))
        | Equal (e1, e2) -> (exp_to_int e1) == (exp_to_int e2);;

eval True;;
eval False;;
eval (Not False);;
eval (Not True);;
eval (AndAlso (True, False));;
eval (AndAlso (True, True));;
eval (OrElse (True, False));;
eval (OrElse (False, False));;
eval (Imply (False, True));;
eval (Imply (False, False));;
eval (Imply (True, True));;
eval (Imply (True, False));;

eval (Imply (Imply (True, False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;

