(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented
exception UndeterminedVariableX
exception InappropirateRange

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
        | Const(a) -> Const(0)
        | Var(s) -> if s = var then Const(1) else Const(0)
        | Power(s, a) -> if s <> var then Const(0) else Times [Const(a); Power(s, a-1)]
        | Times (lst) -> (match lst with
                        | [] -> Const(0)
                        | hd::tl -> Sum [Times (diff(hd, var)::tl); Times [hd; diff(Times tl, var)]])
        | Sum (lst) -> match lst with
                | [] -> Const(0)
                | hd::tl -> Sum [diff(hd, var); diff(Sum (tl), var)]
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

  let rec getWeight: mobile -> int
  = fun mob -> match mob with
  | SimpleBranch(al,aw), SimpleBranch(bl,bw) -> aw + bw
  | SimpleBranch(al,aw), CompoundBranch(bl,bm) -> aw + getWeight (bm)
  | CompoundBranch(al,am), SimpleBranch(bl,bw) -> getWeight (am) + bw
  | CompoundBranch(al,am), CompoundBranch(bl,bm) -> getWeight(am) + getWeight(bm)
  let balanced : mobile -> bool
  = fun mob -> match mob with
  | SimpleBranch(al, aw), SimpleBranch(bl, bw) -> if al*aw = bl*bw then true else false
  | SimpleBranch(al, aw), CompoundBranch(bl, bm) -> if al*aw = bl*getWeight(bm) then true else false
  | CompoundBranch(al,am), SimpleBranch(bl, bw) -> if al*getWeight(am) = bl*bw then true else false
  | CompoundBranch(al, am), CompoundBranch(bl, bm) -> if al*getWeight(am) = bl*getWeight(bm) then true else false
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

  type env = Env of int list

  let length = fun x -> match x with
                        |Env(a) -> List.length a
  let first = fun x -> match x with
                        | Env(a) -> (match a with
                                |[] -> raise NotImplemented
                                |hd::tl -> hd)

  let rec envCalculator: env * exp -> int
  = fun (env, exp) -> match exp with
  | X -> if (length env) = 0 then raise UndeterminedVariableX else first env
  | INT(a) -> a
  | ADD(a,b) -> envCalculator(env, a) + envCalculator(env, b)
  | SUB(a,b) -> envCalculator(env, a) - envCalculator(env, b)
  | MUL(a,b) -> envCalculator(env, a) * envCalculator(env, b)
  | DIV(a,b) -> envCalculator(env, a) / envCalculator(env, b)
  | SIGMA(a,b,c) -> let trueA = envCalculator(env,a) in
                        let trueB = envCalculator(env,b) in
                        if trueA > trueB then raise InappropirateRange else
                                if trueA = trueB then envCalculator(Env([trueA]), c)
                                else envCalculator(Env([trueA]), c) + envCalculator(env, SIGMA(ADD(INT(1),a),b,c))

  let calculator : exp -> int
  = fun exp -> envCalculator(Env([]), exp)
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

  type env = Env of var list
  let rec envHasIt = fun (env, var) -> match env with
                                | Env(lst) -> (match lst with 
                                                | [] -> false
                                                | hd::tl -> if hd = var then true else
                                                false || envHasIt(Env(tl), var))
  let addVarToEnv: env * var -> env 
  = fun (env, var) -> match env with
        | Env(lst) -> Env(lst@[var])

  let rec envCheck : env * exp -> bool 
  = fun (env, exp) -> match exp with
  | V(v) -> envHasIt(env, v)
  | P(v, e) -> envCheck(addVarToEnv(env, v), e)
  | C(e1, e2) -> envCheck(env, e1) && envCheck(env, e2)
  let check : exp -> bool
  = fun exp -> envCheck(Env([]), exp)
end
