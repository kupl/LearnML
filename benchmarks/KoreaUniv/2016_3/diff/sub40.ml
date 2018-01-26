(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

(* Name : Jungwon Seo / Student ID : 2012210051 *)

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

  let compareStr : string * string -> bool
 = fun (str1, str2) -> str1=str2

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> raise NotImplemented
(* match (exp, var) with
 | (Const c, _) -> Const 0
 | (Var x, var) -> if compareStr(x, var) then Const 1 else Const 0
 | (Power(str, n), var)  
   -> if compareStr(str, var) then Times[Const n; Power(str, n-1)] else Const 0
 | (Times (hd::tl), var) ->
	match hd with
	| Var var -> Times([Const 1]@[diff(Times tl, var)])
	| Power(str, n) -> Times([Const n; Power(str, n-1)]@[diff(Times tl, var)])
	| _ -> Times([hd]@[diff(Times tl, var)])
 | (Sum(hd::tl), var) -> Sum(diff(hd, var)::[diff(Sum tl, var)])*)
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

  let rec getTotalW : mobile -> int
 = fun submob ->
 match submob with
 | (SimpleBranch(_, wgh1), SimpleBranch(_, wgh2)) -> wgh1 + wgh2
 | (SimpleBranch(_, wgh1), CompoundBranch(_, submob2)) -> wgh1 + getTotalW(submob2)
 | (CompoundBranch(_, submob1), SimpleBranch(_, wgh2)) -> getTotalW(submob1)+wgh2
 | (CompoundBranch(_, submob1), CompoundBranch(_, submob2)) -> getTotalW(submob1) + getTotalW(submob2)
 
  let rec balanced : mobile -> bool
  = fun mob -> 
 match mob with
 | (SimpleBranch(len1, wgh1),SimpleBranch(len2, wgh2)) -> (len1*wgh1)=(len2*wgh2)
 | (SimpleBranch(len1, wgh1),CompoundBranch(len2, submob2))
	-> (balanced submob2) && ((len1*wgh1)=len2*getTotalW(submob2))
 | (CompoundBranch(len1, submob1), SimpleBranch(len2, wgh2))
	-> (balanced submob1) && (len1*getTotalW(submob1)=(len2*wgh2))
 | (CompoundBranch(len1, submob1), CompoundBranch(len2, submob2))
	-> (balanced submob1) && (balanced submob2) && (len1*getTotalW(submob1)=len2*getTotalW(submob2)) 
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
(*
  let rec getSum : int * int * exp -> int
  = fun (st, ed, expr) ->
	if (st=ed) then let X=st in expr
	else getSum((st+1), ed, expr) + (let X=st in expr)
*)
  let rec calculator : exp -> int
  = fun exp -> raise NotImplemented
(* match exp with
 | INT n -> n
 | ADD(n1, n2) -> calculator(n1) + calculator(n2)
 | SUB(n1, n2) -> calculator(n1) - calculator(n2)
 | MUL(n1, n2) -> calculator(n1) * calculator(n2)
 | DIV(n1, n2) -> calculator(n1) / calculator(n2)
 | SIGMA(st, ed, exp) -> getSum(calculator(st), calculator(ed), exp)*)
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
 
  let listP = [""]

  let rec check : exp -> bool
  = fun exp ->
 match exp with
 | V x -> List.mem x listP
 | P (x, e1) -> let listP = listP @ [x] in check e1 
 | C (e1, e2) -> check e1 && check e2
end
