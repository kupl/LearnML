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
  = fun (exp, var) -> match exp with 
    | Const c -> Const 0
    | Var v -> if (v = var) then Const 1 else Const 0
    | Power (st, i) -> if(st = var) then Times[Const i; Power(st, i - 1)] else Power(st, i)
    | Times(h::t) -> h
    | Sum(h::t) -> let diffHelp = fun (alexp) -> diff(alexp, var) in
      Sum(diff(h,var) :: List.map diffHelp t)
    | _ -> exp
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

  let rec computeWeight : mobile -> int
  = fun mob -> match mob with
    | (SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> ll * lw + rl * rw
    | (SimpleBranch(ll,lw), CompoundBranch(rl,rm)) -> ll * lw + rl * computeWeight(rm)
    | (CompoundBranch(ll,lm), SimpleBranch(rl,rw)) -> ll * computeWeight(lm) + rl * rw
    | (CompoundBranch(ll,lm), CompoundBranch(rl,rm)) -> ll * computeWeight(lm) + rl * computeWeight(rm);;
  let rec balanced : mobile -> bool
  = fun mob -> match mob with
    | (SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> if(ll * lw = rl * rw) then true else false
    | (SimpleBranch(ll,lw), CompoundBranch(rl,rm)) -> if(ll * lw = rl * computeWeight(rm)) then balanced(rm) && true else false
    | (CompoundBranch(ll,lm), SimpleBranch(rl,rw)) -> if(ll * computeWeight(lm) = rl * rw) then balanced(lm) && true else false
    | (CompoundBranch(ll,lm), CompoundBranch(rl,rm)) -> if(ll * computeWeight(lm) = rl * computeWeight(rm)) then balanced(lm) && balanced(rm) else false;;

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
  = fun exp -> match exp with 
    | X -> 0
    | INT v -> v
    | ADD(exp1, exp2) -> calculator(exp1) + calculator(exp2)
    | SUB(exp1, exp2) -> calculator(exp1) - calculator(exp2)
    | MUL(exp1, exp2) -> calculator(exp1) * calculator(exp2)
    | DIV(exp1, exp2) -> calculator(exp1) / calculator(exp2)
    | SIGMA(exp1, exp2, exp3) -> 0
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

  let check : exp -> bool
  = fun exp -> raise NotImplemented (* TODO *)
end

