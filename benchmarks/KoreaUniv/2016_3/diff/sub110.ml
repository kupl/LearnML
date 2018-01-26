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
  = fun (exp, var) -> (* raise NotImplemented  TODO *)
   match exp with
   | Const n -> Const 0
   | Var str -> if str = var then Const 1 else Const 0
   | Power(str,n) ->
      if str = var then(
      if n=0 then Const 0
      else if n=1 then Const 1
      else Times([Const n;Power(str,(n-1))])
      )
      else Const 0
   | Times(lst) ->(
      match lst with
      |[] -> Times([])
      |hd::tl ->
         if tl = [] then diff(hd,var)
         else Sum (Times(diff(hd,var)::tl)::[Times(hd::[diff(Times(tl),var)])])
      )
   | Sum (lst) ->
      match lst with
      |[] -> Sum([])
      |hd::tl -> Sum (diff(hd,var)::[diff(Sum(tl),var)])
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

   let rec mobilew : mobile -> int
   = fun mob ->
   match mob with
   | (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
   | (SimpleBranch(l1,w1),CompoundBranch(l2,mobile)) -> w1 + (mobilew mobile)
   | (CompoundBranch(l1,mobile),SimpleBranch(l2,w2)) -> (mobilew mobile)+w2
   | (CompoundBranch(l1,mobile1),CompoundBranch(l2,mobile2)) -> (mobilew mobile1) + (mobilew mobile2)

  let rec balanced : mobile -> bool
  = fun mob -> (* TODO *)
   match mob with
   | (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if (l1*w1) = (l2*w2) then true else false

   | (SimpleBranch(l1,w1),CompoundBranch(l2,mobile2)) -> 
      if balanced(mobile2) = false then false 
      else if (l1*w1) = (l2 * (mobilew mobile2)) then true else false

   | (CompoundBranch(l1,mobile1),SimpleBranch(l2,w2)) ->
      if balanced(mobile1) = false then false
      else if (l1 * (mobilew mobile1)) = (l2*w2) then true else false

   | (CompoundBranch(l1,mobile1),CompoundBranch(l2,mobile2)) ->
      if balanced(mobile1) = false || balanced(mobile2) = false then false
      else if (l1 * (mobilew mobile1)) = (l2 * (mobilew mobile2)) then true else false

end

(*********************)
(*     Problem 3     *)
(*********************)
exception IllegalInput
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

   let rec sigma2 : exp * int -> int
   = fun (exp,n) ->
   match exp with
   | X -> n
   | INT (n) -> n
   | ADD(e1,e2) -> sigma2(e1,n)+sigma2(e2,n)
   | SUB(e1,e2) -> sigma2(e1,n)-sigma2(e2,n)
   | MUL(e1,e2) -> sigma2(e1,n)*sigma2(e2,n)
   | DIV(e1,e2) -> sigma2(e1,n)/sigma2(e2,n)
   | SIGMA(e1,e2,e3) ->
      if sigma2(e1, n) > sigma2(e2, n) then raise IllegalInput
      else if sigma2(e1, n) = sigma2(e2, n) then sigma2(e3, sigma2(e1, n))
      else sigma2(e3, sigma2(e1, n)) + sigma2(SIGMA(INT (sigma2(e1, n) +1), INT (sigma2(e2, n)), e3),n)

   and calculator : exp -> int
  = fun exp -> (* TODO *)
   match exp with
   | X -> raise IllegalInput
   | INT(n) -> n
   | ADD(e1,e2) -> calculator(e1)+calculator(e2)
   | SUB(e1,e2) -> calculator(e1)-calculator(e2)
   | MUL(e1,e2) -> calculator(e1)*calculator(e2)
   | DIV(e1,e2) -> calculator(e1)/calculator(e2)
   | SIGMA(e1,e2,e3) ->
      if calculator(e1) > calculator(e2) then raise IllegalInput
      else if calculator(e1) = calculator(e2) then sigma2(e3, calculator(e1))
      else sigma2(e3, calculator(e1)) + calculator(SIGMA(INT (calculator(e1) +1), INT (calculator(e2)), e3))
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

   let rec strbool : string * string list -> bool
   = fun(var, strlist) ->
   match strlist with
   | [] -> false
   | hd::tl -> if hd = var then true else strbool(var, tl)

   let rec expbool : exp * string list -> bool
   = fun(exp, strlist) ->
   match exp with
   | V(var) -> strbool(var, strlist)
   | P(var, exp) -> expbool(exp, var::strlist)
   (*| P(var, exp) -> expbool(exp, [var::strlist])*)
   | C(e1, e2) -> if (expbool(e1, strlist) = true) && (expbool(e2, strlist) = true) then true else false

  let rec check : exp -> bool
  = fun exp -> (* raise NotImplemented  TODO *)
   match exp with
   | V(var) -> false
   | P(var, exp) -> expbool(exp, [var])
   (*| P(var, exp) -> expbool(exp, var)*)
   | C(e1, e2) -> if ((expbool(e1, []) && expbool(e2, [])) = true) then true else false
   (*| C(e1, e2) -> if ((expbool(e1, []) && (expbool(e2, [])) = true then true else false
 | C(e1, e2) -> if (expbool(e1, []) = true) && (expbool(e2, []) = true) then true else false*)
end
