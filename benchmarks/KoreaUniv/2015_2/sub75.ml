(* Problem 1: filter *)
let rec filter pred lst =
	match lst with
	|[] -> []
	|hd::tl -> if pred(hd) then hd::(filter pred tl) 
	else (filter pred tl);;


(* Problem 2: zipper *)
let rec zipper : int list * int list -> int list
=fun (a,b) ->match b with
      |[] -> a
      |hd::tl -> match a with
            |[] -> b
            |hd2::tl2 -> [hd2]@[hd]@zipper (tl2,tl);;


(* Problem 3: iter *)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f
let rec iter (n,f) a =
   if n=0 then a else f (iter(n-1,f) a);;


(* Problem 4: Diff *)
type aexp = 
	| Const of int 
	| Var of string 
	| Power of string * int 
	| Times of aexp list 
	| Sum of aexp list 
let rec diff : aexp * string -> aexp =fun (aexp,x) -> aexp 
 

(* Problem 5: Calculator *)
type exp = 
	X 
	| INT of int 
	| ADD of exp * exp 
	| SUB of exp * exp 
	| MUL of exp * exp 
	| DIV of exp * exp 
	| SIGMA of exp * exp * exp 
let calculator : exp -> int =fun e -> 0  