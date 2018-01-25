
(* Problem 1: filter *)
let rec filter pred lst =
  match lst with
  | [] -> []
  | hd::tl -> if pred hd = false then (filter pred tl)
  else hd::(filter pred tl)


(* Problem 2: zipper *)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
  match (a,b) with
  |(_, []) -> a
  |([], _) -> b
  |(h1::t1,h2::t2) -> 
  if h1>h2 then zipper(h2::a,t2)
  else if h1<h2 then h1::zipper(t1,b)
  else zipper(a, t2)

(* Problem 3: iter *)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) num -> 
  match (n,f) with
  |(0,_) -> 0
  |(1,_) -> f num
  |(_,_) -> if n>1 then let num = f num in iter(n-1,f) num
  else 0

(* Problem 4: Diff   *)

type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (aexp,x) -> 
match aexp with
|Const int -> Const 0
|Var v-> 
if x = v then Const 1
 else Const 0
|Power (v, aexp') ->
 if x=v then Times [Const aexp'; Power (v, aexp'-1)]
 else Const 0
|Times lst ->
(match lst with
|[] -> Const 0
|hd::tl -> Sum [Times (diff (hd, x)::tl); Times [hd; diff (Times tl, x)]])
|Sum list ->
(match list with
|[] -> Const 0
|hd::tl -> Sum [diff(hd,x); diff(Sum tl, x)])


(* Problem 5: Calculator *)
exception FreeVariable
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal : exp -> int
=fun exp ->
  match exp with
  | X -> raise FreeVariable
  | INT e1 -> e1
  | ADD(e1, e2) -> cal e1 + cal e2
  | SUB(e1, e2) -> cal e1 - cal e2
  | MUL(e1, e2) -> cal e1 * cal e2
  | DIV(e1, e2) -> cal e1 / cal e2
  | SIGMA(p, q, f) ->
	if (cal p)>(cal q) then 0 else eval(f,p) + cal(SIGMA(ADD(p,INT 1),q,f))
  and eval (f,x) = 
	 match f with
  | X -> cal x
  | INT z -> z
  | ADD(f1,f2) -> eval (f1,x) + eval (f2,x)
  | SUB(f1,f2) -> eval (f1,x) - eval (f2,x)
  | MUL(f1,f2) -> eval (f1,x) * eval (f2,x)
  | DIV(f1,f2) -> eval (f1,x) / eval (f2,x)
  | SIGMA (i,j,f1) -> cal (SIGMA (INT (eval (i,x)), INT (eval (j,x)), f1))