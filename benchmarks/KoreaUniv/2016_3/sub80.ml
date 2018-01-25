(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
  
  let rec map f l =
  match l with
  |[] -> []
  |h::t -> (f h)::(map f t)
  
  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  |Const x -> Const 0
  |Var x -> if x = var then Const 1 else Const 0
  |Power (str, n) -> if str = var then Times [Const n; Power (str, n - 1)] else Const 0
  |Times [] -> Const 0
  |Times (h::t) -> Sum [Times (diff (h,var)::t); Times [h; diff(Times t, var)]]
  |Sum x -> Sum (map (fun k -> diff (k, var)) x)
  
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec branchWeight : branch -> weight 
  =fun x ->
  match x with
  |SimpleBranch(l,w) -> w
  |CompoundBranch(l,w) -> let rec mobileWeight : mobile -> int
                          =fun y ->
                          match y with
                          |(a,b) -> branchWeight(a) + branchWeight(b) in mobileWeight(w)

  let rec mobileWeight : mobile -> int
  =fun m ->
  match m with
  |(a,b) -> branchWeight(a) + branchWeight(b)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
  (lb, rb) -> match lb with
              |SimpleBranch (l1,w1) ->
              (match rb with
                |SimpleBranch (l2,w2) ->
                    if (l1*w1=l2*w2) then true else false
                |CompoundBranch (l2,w2) ->
                    if (balanced(w2)) then if (l1*w1=l2*mobileWeight(w2)) then true else false else false
              )
              |CompoundBranch (l1,w1) -> 
              (match rb with
                |SimpleBranch (l2,w2) ->
                    if (balanced(w1)) then if (l2*w2=l1*mobileWeight(w1)) then true else false else false
                |CompoundBranch (l2,w2) -> 
                    if (balanced(w1)&&balanced(w2))  then if (l2*mobileWeight(w2)=l1*mobileWeight(w1)) then true else false else false
              )
end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
type exp =
| X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp ->
let rec calc exp num fl output = (
	match exp with
	X-> if fl = 0 then raise(Failure "match failure") else num
	| INT x->x
	| ADD(x, y)-> (calc x num fl output) + (calc y num fl output)
	| SUB(x, y)-> (calc x num fl output) - (calc y num fl output)
	| MUL(x, y)-> (calc x num fl output)*(calc y num fl output)
	| DIV(x, y)-> if (calc y num fl output) = 0 then raise(Failure "denominator is zero") else (calc x num fl output) / (calc y num fl output)
	| SIGMA(x, y, z)->
	let num = calc x num fl output in
	let fl = 1 in
	(if (calc x num fl output)>(calc y num fl output) then output
	else let output = output + (calc z num fl output) in
		calc(SIGMA(INT((calc x num fl output) + 1), INT(calc y num fl output), z)) num fl output)
	) in calc exp 0 0 0
	end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
  
  let rec exist v lst = match lst with 
  | [] -> false
  | hd::tl ->  if (v = hd) then true else ( exist v tl) 
 
  let rec checker e lst = match e with
  | V (v) -> exist v lst 
  | P (v,e) -> checker e (lst@[v])
  | C(e1,e2) ->if ((checker e1 lst) = (checker e2 lst)) then true else false

  let rec check : exp -> bool
  = fun exp -> checker exp []
end

