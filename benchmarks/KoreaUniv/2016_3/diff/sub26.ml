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
  | Sum of aexp list;;

let rec hasVar
= fun (exp, var) ->
match exp with
  | hd::tl -> (match hd with
               | Const x -> false
               | Var x -> if x = var then true else false
               | Power (x, y) -> if x = var
                                 then (if y != 0 then true else false)
                                 else false
               | Times l -> hasVar (l, var)
               | Sum l -> hasVar (l, var)
              ) || hasVar(tl, var)
  | [] -> false;;

  let rec diff : aexp * string -> aexp
    = fun(exp, var) -> match exp with
      | Power (x, y) -> if x = var then
                          (if y = 0 then Const 0
                           else if y = 1 then Const y
                           else Times [Const y; Power(x, y-1)])
                        else (if y = 0 then Var x else Power(x,y))
      | Const x -> Const 0
      | Var x -> if x = var then Const 1 else Const 0                                                   
      | Sum l -> (match l with
                  | [] -> Const 0
                  | [x] -> diff (x, var)
                  | hd::tl -> Sum[diff (hd,var); diff(Sum tl,var)])
      | Times l -> let flag = hasVar(l,var) in
                   (match l with 
                    | [] -> Const 0
                    | [x] -> if flag = true then (* Var exists! *)
                             (if diff(x,var) = Const 0 then x else diff(x,var))
                             else Const 0
                    | hd::tl -> Times [(if flag = true then
                                         (if diff(hd,var) = Const 0
                                          then hd else diff(hd,var))
                                        else Const 0); diff(Times tl,var)]
                   );;
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

let rec getWeight branch =
  match branch with
    | SimpleBranch (x, y) -> y
    | CompoundBranch (x, (left, right)) -> getWeight(left) + getWeight(right);;

let getTorque branch =
  match branch with
    | SimpleBranch (x, y) -> x * y
    | CompoundBranch (x, (left, right)) -> x * (getWeight(left) + getWeight(right));;

  let balanced : mobile -> bool
  = fun mob -> match mob with
               | (left, right) ->
                 if getTorque(left) = getTorque(right) then true else false;;
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
;;

let rec evalSigma(lo, hi, op) =
if lo > hi then 0
else
let flag = (if lo < hi then 1 else 0) in
match op with
  | X -> lo + flag * evalSigma(lo+1, hi, op)
  | INT c -> c + flag * evalSigma(lo+1, hi, op)
  | ADD (x, y) -> evalSigma(lo, lo, x) + evalSigma(lo, lo, y) + flag * evalSigma(lo+1, hi, op)
  | SUB (x, y) -> evalSigma(lo, lo, x) - evalSigma(lo, lo, y) + flag * evalSigma(lo+1, hi, op)
  | MUL (x, y) -> evalSigma(lo, lo, x) * evalSigma(lo, lo, y) + flag * evalSigma(lo+1, hi, op)
  | DIV (x, y) -> evalSigma(lo, lo, x) / evalSigma(lo, lo, y) + flag * evalSigma(lo+1, hi, op)
  | SIGMA(INT st, INT ed, x) -> evalSigma(st, ed, x) + flag * evalSigma(lo+1, hi, op)
  | _ -> 0;;


  let calculator : exp -> int
  = fun exp -> match exp with
  | INT x -> x
  | ADD (INT x, INT y) -> x + y
  | SUB (INT x, INT y) -> x - y
  | MUL (INT x, INT y) -> x * y
  | DIV (INT x, INT y) -> x / y
  | SIGMA (INT lo, INT hi, op) -> evalSigma(lo, hi, op)
  | _ -> raise NotImplemented;;

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
;;

let rec evalCheck1 ex bd = 
match ex with
  |V x -> false
  |P (arg, body) -> if arg = bd then true else (evalCheck1 body bd)
  |C (a, b) -> (evalCheck1 a bd) || (evalCheck1 b bd)
;;
let rec evalCheck2 ex bd =
match bd with
  |V x -> (evalCheck1 ex x)
  |P (arg, body) -> (evalCheck2 ex body)
  |C (a, b) -> (evalCheck2 ex a) && (evalCheck2 ex b)
;;

  let rec check : exp -> bool
  = fun exp -> match exp with
    |V x -> false
    |P (arg, body) -> (evalCheck2 exp body)
    |C (a, b) -> (check a) && (check b)
;;

end

