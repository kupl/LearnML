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
      | Const a -> Const 0
      | Var a -> if var = a then Const 1 else Const 0
      | Power (a, b) -> 
          (if a <> var then Const 0
          else (Times[Const b; Power(a, b-1)]))
      | Times [] -> Const 0
      | Times (hd::tl) -> Sum [Times (diff (hd, var)::tl); Times [hd; diff(Times tl, var)]]
      | Sum a -> Sum (List.map (fun ae -> diff (ae,var)) a)
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

  let rec weight a =
  match a with
  | (SimpleBranch(x, y), SimpleBranch(w, z)) -> y + z
  | (SimpleBranch(x, y), CompoundBranch(w, z)) -> y + (weight z)
  | (CompoundBranch(x, y), SimpleBranch(w, z)) -> (weight y) + z
  | (CompoundBranch(x, y), CompoundBranch(w, z)) -> (weight y) + weight z

  let rec balanced : mobile -> bool
  = fun mob -> 
  match mob with
  | (SimpleBranch(x, y), SimpleBranch(w, z)) -> if x*y = w*z then true else false 
  | (SimpleBranch(x, y), CompoundBranch(w, z)) -> if (x*y = w*(weight z)) && balanced z then true else false
  | (CompoundBranch(x, y), SimpleBranch(w, z)) -> if (x*(weight y) = w*z) && balanced y then true else false
  | (CompoundBranch(x, y), CompoundBranch(w, z)) -> if (x*(weight y) = w*(weight z)) && balanced y && balanced z then true else false 
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

  let empty_env = []

  let rec lookup_env env var = 
    match env with
    | [] -> false
    | hd::tl -> if hd = var then true else lookup_env tl var


  let extend_env env v = v::env

  let check : exp -> bool
  = fun exp -> 
  let rec c_env env ex = 
  match ex with
  | V a -> lookup_env env a
  | P (a, b) -> let env2 = extend_env env a in c_env env2 b
  | C (a, b) -> if c_env env a && c_env env b then true else false
  in c_env empty_env exp

end

