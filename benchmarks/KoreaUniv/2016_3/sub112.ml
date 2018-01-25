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
| Const _ -> Const 0
| Var y -> if var = y then Const 1 else Const 0
| Power (y,n) -> if not (var = y) then Const 0
									else Times [Const n; Power (y,n-1)]
| Times [] -> Const 0
| Times (hd::tl) -> Sum [Times (diff (hd,var)::tl); Times [hd; diff(Times tl, var)]]
| Sum aexp_bunch -> Sum (List.map (fun ae_temp -> diff(ae_temp,var)) aexp_bunch);;

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
  = fun mob -> false;; 
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

  let rec  calculator : exp -> int
  = fun exp -> 375;; 





     
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
  = fun exp -> false;; 
end

