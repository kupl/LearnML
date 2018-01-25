(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
(*
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
   | Const a -> Const 0
   | Var x -> if x = var then Const 1 else Var x
   | Power (x, a) -> if x = var then Times[Const a; Power (x, a-1)] else Power (x, a)
   | Times l ->
          (match l with
          | hd::tl -> Sum(Times(diff (hd, var)::tl); diff (Times tl, var))
          )
   | Sum l ->
          (match l with
          | hd::tl -> Sum(diff (hd, var)::diff (tl, var))
          )

end*)
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
    
  let rec balanced : mobile -> bool
  = fun mob -> 
    match mob with
    | SimpleBranch (a, b), SimpleBranch (x, y) -> if a*b=x*y then b+y else false
    | CompoundBranch (a, b), SimpleBranch (x, y) -> match b with
             | SimpleBranch (x, y), SimpleBranch (l, m) -> if x*y=l*m then balanced SimpleBranch(a, y+m), SimpleBranch(x, y) else false
             | CompoundBranch (x, y), SimpleBranch (l, m) -> balanced CompoundBranch (x, ((balanced CompoundBranch (x, y)), SimpleBranch (l, m)))
             | SimpleBranch (x, y), CompoundBranch (l, m, n) -> balanced CompoundBranch (balanced SimpleBranch (x, y), (balanced CompoundBranch (l, m, n)))
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
  = fun exp -> raise NotImplemented  (* TODO *)
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

