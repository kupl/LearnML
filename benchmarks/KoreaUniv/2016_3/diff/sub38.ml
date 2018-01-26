(*********************)
(*     Problem 1     *)
(*********************)

(* X^n -> n x X^(n-1)
ss mb -> 0
(ab) mb = a mb * b + b mb * a *)


type aexp = 
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list


let rec diff : aexp* string -> aexp = fun (exp, var) -> 
   match exp with
  |Const a-> Const 0
  |Var b -> if b = var then Const 1 else Const 0
  |Power (s, n) ->
  if s = var then Times [Const n ; Power (s, n - 1 )] else Const 0
  |Times [] -> Const 0
  |Times (h::t) -> 
	Sum [Times ((diff(h, var))::t); Times[h; diff(Times t, var)]]
  |Sum [] -> Const 0
  |Sum (h::t) -> Sum [diff(h,var); diff(Sum t, var)]




(*********************)
(*     Problem 2     *)
(*********************)

type mobile = branch * branch
and branch = SimpleBranch of length * weight
	    |CompoundBranch of length * mobile
and length = int
and weight = int

let rec rweight m = match m with
| (SimpleBranch (x,y), SimpleBranch (a,b)) -> if (x*y) = (a*b) then y*b else -1
| (SimpleBranch (x,y), CompoundBranch (a,b)) -> if (x*y)=(a*(rweight b)) then y+(rweight b) else -1
| (CompoundBranch (x,y), SimpleBranch (a,b)) -> if (x*(rweight y)) = (a*b) then (rweight y) + b else -1
| (CompoundBranch (x,y), CompoundBranch (a,b)) -> if (x*(rweight y))=(a*(rweight b)) then ((rweight y)+(rweight b)) else -1

let rec balanced f = 
match f with
| (SimpleBranch (x,y), SimpleBranch(a,b)) -> if (x*y) =(a*b) then true else false
| (SimpleBranch (x,y), CompoundBranch (a,b)) -> if (x*y) = (a*(rweight b)) then true else false
| (CompoundBranch (x,y), SimpleBranch (a,b)) -> if (x*(rweight y)) = (a*b) then true else false
| (CompoundBranch (x,y), CompoundBranch (a,b)) -> if (x*(rweight y)) = (a*(rweight b)) then true else false;;


(*********************)
(*     Problem 3     *)
(*********************)
type exp = X
	|INT of int
	|ADD of exp * exp
	|SUB of exp * exp
	|MUL of exp * exp
	|DIV of exp * exp
	|SIGMA of exp * exp * exp


let rec calculator : exp-> int =
fun f ->
match f with
|X-> 0
|INT a -> a
|ADD (e1, e2) -> calculator (e1) + calculator (e2)
|SUB (e1, e2) -> calculator (e1) - calculator (e2)
|MUL (e1, e2) -> calculator (e1) * calculator (e2)
|DIV (e1, e2) -> calculator (e1) / calculator (e2)
|SIGMA (e1, e2, e3) ->
match e3 with
  |INT a -> calculator(MUL(ADD(SUB(e2,e1),INT 1),e3))
  |X -> calculator(SUB(DIV(MUL(e2,ADD(e2,INT 1)),INT 2), DIV(MUL(e1,SUB(e1,INT 1)),INT 2)))
 (* |ADD (a, b)
if calculator (a) = X then let q = calculator(e2) else let q = calculator (a) in
if calculator (b) = X then let p = calculator(e2) else calculator (b) in

  |SUB (a, b) ->
  |MUL (a, b)
  |DIV (a, b)
in 



 let x = calculator(e2) in calculator(e3)
let x = calculator (e1) in 


(*********************)
(*     Problem 4     *)
(*********************)

type exp = V of var
	| P of var * exp
	| C of exp * exp


let rec chekc : exp -> bool
= fun f ->
*)
