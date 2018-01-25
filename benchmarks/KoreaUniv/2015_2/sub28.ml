(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
	match lst with
	| [] -> []
	| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
| [] -> b
| hd::tl -> hd::zipper(b,tl)

(*******************)
(* Problem 3: iter *)
(*******************)
let id = fun x -> x
let composite f g = fun x -> g(f x)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> if n=0 then id else composite f (iter(n-1,f))

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
| Const(i) -> Const(0)
| Var(s) -> if s=x then Const(1) else Const(0)
| Power(s, i) -> (
	if s=x then (
	match i with
	| 0 -> Const(0)
	| 1 -> Const(1)
	| 2 -> Times[Const(2); Var(s)]
	| _ -> Times[Const(i); Power("x", i-1)]) else Const(0)
	)
| Times(l) -> (
	match l with
	| [] -> Const(0)
	| hd::tl-> Sum([Times([diff(hd,x)] @ tl)] @ [Times([hd] @ [diff(Times(tl),x)])])
	)
| Sum(l) -> (
	match l with
	| [] -> Const(0)
	| hd::tl -> Sum([diff(hd,x)] @ [diff(Sum(tl),x)])
	)

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator_x
=fun e x -> match e with
| X ->
	(match x with
	| None -> raise (Failure "error")
	| Some s -> s)
| INT i -> i
| ADD(e1,e2) -> (calculator_x e1 x)+(calculator_x e2 x)
| SUB(e1,e2) -> (calculator_x e1 x)-(calculator_x e2 x)
| MUL(e1,e2) -> (calculator_x e1 x)*(calculator_x e2 x)
| DIV(e1,e2) -> (calculator_x e1 x)/(calculator_x e2 x)
| SIGMA(e1,e2,e3) ->
	let a = calculator_x e2 x in
	let rec sigma b f =
		if b > a then f
		else (let v = calculator_x e3 (Some b) in
			sigma (b+1) (v+f)) in
		sigma (calculator_x e1 x) 0 
let calculator : exp -> int
=fun e -> calculator_x e None