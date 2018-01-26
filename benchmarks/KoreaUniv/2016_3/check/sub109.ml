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

let rec p1_help (aexp, var)
= match aexp with
|[]->[]
|hd::tl -> (diff (hd,var)::p1_help (tl,var))

and diff : aexp * string -> aexp
  = fun (aexp, var) -> 
match aexp with
|Const n -> Const 0
|Var y ->
(
if y=var then Const 1
else Const 0
)
|Power (y,n) ->
(
if y=var then Times [Const n; Power (y,n-1)]
else  Const 0
)
|Times aexp1 -> 
(
match aexp1 with
|[] -> Const 0
|hd::tl -> Sum [(Times ((diff (hd,var))::tl));(Times (hd::p1_help (tl,var)))]
)
|Sum aexp1 -> Sum (p1_help(aexp1,var))
 
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


let rec cal_weight mob=
 match mob with
 |(SimpleBranch (a1,b1),SimpleBranch (a2,b2)) ->b1+b2
 |(SimpleBranch (a1,b1),CompoundBranch (a2,b2)) -> b1 + (cal_weight b2)
 |(CompoundBranch (a1,b1), SimpleBranch (a2,b2))-> (cal_weight b1) + b2
 |(CompoundBranch (a1,b1), CompoundBranch (a2,b2))-> 
(cal_weight b1)+(cal_weight b2)

and balanced : mobile -> bool
  = fun mob->
match mob with 
|(SimpleBranch (a1,b1),SimpleBranch (a2,b2)) ->
if (a1<0)||(b1<0)||(a2<0)||(b2<0) then raise NotImplemented
else a1*b1==a2*b2
|(SimpleBranch (a1,b1),CompoundBranch (a2,b2)) ->
 if (a1<0)||(b1<0)||(a2<0) then raise NotImplemented
else (a1*b1==a2*(cal_weight b2))&&(balanced b2)
|(CompoundBranch (a1,b1), SimpleBranch (a2,b2)) ->
 if (a1<0)||(a2<0)||(b2<0) then raise NotImplemented
else (a1*(cal_weight b1)==a2*b2)&&(balanced b1)
|(CompoundBranch (a1,b1), CompoundBranch (a2,b2)) ->
 if (a1<0)||(a2<0) then raise NotImplemented
else
 ((a1*(cal_weight b1))==(a2*(cal_weight b2)))&&(balanced b1)&&(balanced b2)

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
match exp with
|X -> raise NotImplemented
|INT k -> k
|ADD (exp1,exp2) -> calculator exp1 + calculator exp2
|SUB (exp1,exp2) -> calculator exp1 - calculator exp2
|MUL (exp1,exp2) -> (calculator exp1) * (calculator exp2)
|DIV (exp1,exp2) -> (calculator exp1) / (calculator exp2)
|SIGMA (exp1,exp2,exp3) ->
if (calculator exp1)>(calculator exp2) then raise NotImplemented
else if (calculator exp1)<(calculator exp2) then 
(eval_sigma (calculator exp1) exp3) + 
calculator (SIGMA (INT (1+(calculator exp1)), exp2, exp3))
else eval_sigma (calculator exp2) exp3


and eval_sigma x exp3
= match exp3 with
|X->x
|INT k -> k
|ADD (exp1,exp2) -> (eval_sigma x exp1) + (eval_sigma x exp2)
|SUB (exp1,exp2) -> (eval_sigma x exp1) - (eval_sigma x exp2)
|MUL (exp1,exp2) -> (eval_sigma x exp1) * (eval_sigma x exp2)
|DIV (exp1,exp2) -> (eval_sigma x exp1) / (eval_sigma x exp2)
|_-> raise NotImplemented

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

let rec var_find exp =
match exp with
|V a -> []
|P (a, exp1) -> a::(var_find exp1)
|C (exp1, exp2) -> (var_find exp1)@(var_find exp2)

let rec exp_find exp =
match exp with
|V a -> [a]
|P (a, exp1) -> exp_find exp1
|C (exp1,exp2) -> (exp_find exp1)@(exp_find exp2)

let rec find s l =
match l with
|[] -> false
|hd::tl -> if s = hd then true else find s tl

let rec find_match l1 l2 = 
match l2 with
|[] -> true
|hd::tl -> (find hd l1)&&(find_match l1 tl)

 let check : exp -> bool
= fun exp ->
find_match (var_find exp) (exp_find exp)
end
