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

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with Const n -> 
Const 0
|Var x -> if x=var then Const 1 else exp
|Power (x, n) -> 
if x=var then Times [Const n; Power (x, n-1)] 
else exp
|Times l -> (match l with 
  [] -> Const 0
  |hd::tl -> Sum([Times(diff(hd, var)::tl)]@[Times(hd::[diff(Times(tl), var)])]))
|Sum l -> (match l with 
  [] -> Const 0
  |hd::tl -> Sum([diff (hd, var)]@[diff (Sum (tl), var)]));; 
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

  let rec weight2 mobile =
    let rec weight1 branch = match branch with
      SimpleBranch (l, w) -> w
      |CompoundBranch (l, m) -> weight2 (m) in
      match mobile with
        (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
        |(SimpleBranch (l1, w1), CompoundBranch (l2, m)) -> w1 + weight1 (CompoundBranch (l2, m))
        |(CompoundBranch (l1, m), SimpleBranch (l2, w2)) -> w2 + weight1 (CompoundBranch (l1, m))
        |(CompoundBranch (l1, w1), CompoundBranch(l2, w2)) -> weight1 (CompoundBranch (l1, w1))+weight1 (CompoundBranch (l2, w2))
  
  let rec weight1 branch = match branch with
    SimpleBranch (l, w) -> w
    |CompoundBranch (l, m) -> weight2 (m)

  let rec balanced2 : mobile -> bool
  =fun (lb,rb) -> match lb with 
    SimpleBranch (l1, w1) -> (match rb with
                              SimpleBranch (l2, w2)->if w1*l1 = w2*l2 then true else false
                              |CompoundBranch (l2, m)->if w1*l1 = weight2(m)*l2 then true else false)
    |CompoundBranch (l1, m1) -> (match rb with
                              SimpleBranch (l2, w)->if l2*w = l1*weight2(m1) then true else false
                              |CompoundBranch (l2, m2)->if l2*weight2 (m2)=l1*weight2(m1) then true else false)

  let balanced : mobile -> bool
  = fun mob -> match mob with 
  | (lb,rb) -> match lb with
    SimpleBranch (l1, w1) -> (match rb with
                              SimpleBranch (l2, w2)-> balanced2 (lb, rb)
                              |CompoundBranch (l2, m)->if ((balanced2 (lb, rb)=true)&&(balanced2 (m)=true)=true) then true else false)
    |CompoundBranch (l1, m1) -> (match rb with
                              SimpleBranch (l2, w)->if ((balanced2 (lb, rb)=true)&&(balanced2 (m1)=true)=true) then true else false
                              |CompoundBranch (l2, m2)->if ((balanced2 (lb, rb)=true)&&(balanced2 (m1)=true)&&(balanced2 (m2)=true)) then true else false)

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

exception Problem 

let rec cal (exp, num) 
= match exp with X -> 
num
|INT (n)-> n
|ADD (exp1, exp2) -> (cal (exp1, num)) + (cal (exp2, num))
|SUB (exp1, exp2) -> (cal (exp1, num)) - (cal (exp2, num))
|MUL (exp1, exp2) -> (cal (exp1, num)) * (cal (exp2, num))
|DIV (exp1, exp2) -> (cal (exp1, num)) / (cal (exp2, num))
|SIGMA (exp1, exp2, exp3) -> 
(match exp1 with 
  INT (m) -> if exp1=exp2 then cal (exp3, m) else cal (exp3, m)+cal (SIGMA (INT (m+1), exp2, exp3), m)
 | _ -> raise Problem);;

  let rec calculator : exp -> int
  = fun exp -> match exp with 
X -> raise Problem
|INT (n) -> n
|ADD (exp1, exp2) -> calculator (exp1) + calculator (exp2)
|SUB (exp1, exp2) -> calculator (exp1) - calculator (exp2)
|MUL (exp1, exp2) -> calculator (exp1) * calculator (exp2)
|DIV (exp1, exp2) -> calculator (exp1) / calculator (exp2)
|SIGMA (exp1, exp2, exp3) -> 
(match calculator (exp1) with m -> if m=calculator(exp2) then cal (exp3, m) else cal (exp3, m)+calculator (SIGMA (INT (m+1), exp2 , exp3)));;
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

  let rec check3 v l= match l with
    [] -> false
    |hd::tl -> if hd=v then true else check3 v tl

  let rec check2 e l = match e with
     V(v)->check3 v l
    |P(v, exp1)->check2 exp1 (l@[v])
    |C(exp2, exp3)->check2 exp2 l&&check2 exp3 l

  let check : exp -> bool
  = fun exp -> check2 exp []
end

