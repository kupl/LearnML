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
      Const c -> Const 0
    | Var s -> if s = var then Const 1 else Const 0
    | Power (s, n) -> begin
		                  if s <> var then Const 0
                      else match n with
                        1 -> Times [Const 1]
                      | _ -> Times [Const n; Power (s, n-1)]
                      end
    | Times l -> begin
                 match l with
                 | hd::tl -> Sum [Times (diff (hd, var)::tl); Times [hd; diff (Times tl, var)]]
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

  let rec total_weight : mobile -> int
  = fun mweight -> match mweight with
      (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
    | (CompoundBranch (a, b), SimpleBranch (l, w)) -> total_weight b + w
    | (SimpleBranch (l, w), CompoundBranch (a, b)) -> w + total_weight  b
    | (CompoundBranch (a1, b1), CompoundBranch (a2, b2)) -> total_weight b1 + total_weight b2

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
      (SimpleBranch(l1, w1), SimpleBranch(l2,w2)) -> if (l1 * w1) == (l2 * w2) then true else false
    | (CompoundBranch (a, b), SimpleBranch(l2, w2)) -> if (a*total_weight b) == (l2*w2) then true else false
    | (SimpleBranch(l1, w1), CompoundBranch(a,b)) -> if (a*total_weight b) == (l1*w1) then true else false
    | (CompoundBranch(a1, b1), CompoundBranch(a2, b2)) -> if (a1*total_weight b1) == (a2*total_weight b2) then true else false
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
    | X -> raise (Failure "MUST PUT INTEGER VALUE")
    | INT i -> i
    | ADD (e1, e2) -> (calculator e1) + (calculator e2)
    | SUB (e1, e2) -> (calculator e1) - (calculator e2)
    | MUL (e1, e2) -> (calculator e1) * (calculator e2)
    | DIV (e1, e2) -> if calculator e2 == 0 then raise (Failure "Division by 0") else (calculator e1) / (calculator e2)
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
  = fun exp -> raise NotImplemented 
end

