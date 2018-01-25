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
	| Const n -> Const 0
	| Var str -> if str = var then Const 1 else Var str 
	| Power (str, i) -> if str = var then Times[Const i; Power(str, i-1)] else Power(str, i)
	| Times lst -> begin
	match lst with
	| [] -> Const 0 
	| hd::tl -> Times [hd ; diff(tl, var)]
end
	| Sum lst -> begin
	match lst with
	| [] -> Const 0
	| hd::tl -> Sum [diff(hd, var) ; (diff(tl, var))]
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
  = fun mob -> raise NotImplemented (* TODO *)
end

(*
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
  = fun exp -> match exp with
| INT n -> n
| ADD (INT n1, INT n2) -> (n1+n2)
| SUB (INT n1, INT n2) -> (n1-n2)
| MUL (INT n1, INT n2) -> (n1*n2)
| DIV (INT n1, INT n2) -> (n1/n2)
| SIGMA (INT n1, INT n2, f) -> 

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
  = fun exp -> match exp with

end
*)
