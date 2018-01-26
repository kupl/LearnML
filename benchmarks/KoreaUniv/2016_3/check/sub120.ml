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
  = fun (exp, var) -> 
  match exp with
  | Const n -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Power (x, n) -> if x = var then Times [Const n; Power (x, n - 1)] else Const 0
  | Times l -> 
    begin
      match l with
      | hd::tl -> if tl<>[] then Sum [Times (diff (hd, var)::tl); Times [hd; diff (Times tl, var)]]
                   else diff (hd, var)
      | [] -> Const 0
    end
  | Sum l -> 
    begin
      match l with
      | hd::tl -> if tl<>[] then Sum [diff (hd, var); diff (Sum tl, var)]
                   else diff (hd, var)
      | [] -> Const 0
    end
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

  let balanced : mobile -> bool
  = fun mob -> 
    let rec subFun : mobile -> weight * bool
    = fun m ->
    match m with
    | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if l1 * w1 = l2 * w2 then (w1 + w2, true)
                                                             else (w1 + w2, false)
    | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> let (mw2, mb2) = subFun m2 in
                                                                if mb2 && (l1 * w1 = l2 * mw2) then (w1 + mw2, true)
                                                                else (w1 + mw2, false)
    | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> let (mw1, mb1) = subFun m1 in
                                                                if mb1 && (l1 * mw1 = l2 * w2) then (mw1 + w2, true)
                                                                else (mw1 + w2, false)
    | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> let (mw1, mb1) = subFun m1 in 
                                                                  let (mw2, mb2) = subFun m2 in
                                                                  if mb1 && mb2 && (l1 * mw1 = l2 * mw2) then (mw1 + mw2, true)
                                                                  else (mw1 + mw2, false)
  in let (mw, mb) = subFun mob in
    if mb then true else false
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

  let calculator : exp -> int
  = fun exp -> 
    let rec subFun : exp * exp -> int
    = fun (var, e) -> 
    match e with
    | X -> subFun (X, var)
    | INT i -> i
    | ADD (e1, e2) -> subFun (var, e1) + subFun (var, e2)
    | SUB (e1, e2) -> subFun (var, e1) - subFun (var, e2)
    | MUL (e1, e2) -> subFun (var, e1) * subFun (var, e2)
    | DIV (e1, e2) -> subFun (var, e1) / subFun (var, e2)
    | SIGMA (current, final, e) -> if current = final then subFun (current, e)
                                       else subFun (var, ADD (INT (subFun(current, e)), SIGMA (INT (subFun (X, current)+1), final, e)))
  in subFun (X, exp)                                 
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

  let rec scan : var -> var list -> bool
  = fun s l ->
  match l with
  | [] -> false
  | hd::tl -> if hd = s then true
               else scan s tl

  let check : exp -> bool
  = fun exp -> 
    let rec subFun : exp * var list  -> bool
    = fun (e, env) ->
    match e with
    | V v -> if scan v env then true
             else false
    | P (v, e) -> subFun (e, v::env)
    | C (e1, e2) -> if subFun (e1, env) && subFun (e2, env) then true
                     else false
  in subFun (exp, [])
end
