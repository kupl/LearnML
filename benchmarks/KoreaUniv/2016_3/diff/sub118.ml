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
  =fun (aexp,x) -> match aexp with
    |Const a -> Const 0
    |Var "x" -> if "x" = x then Const 1 else Const 0
    |Power ("x", a) -> (match a with
      |2 -> Times[Const 2; Var "x"]
      |1 -> Const 1
      |0 -> Const 0
      |_ -> Times[Const a; Power ("x", a-1)])
    |Times l -> (match l with
      | [] -> Const 0
      | [a] -> diff (a,x)
      | h::t -> Sum[Times[diff (h,x);Times t]; Times[h;diff (Times t,x)]])
    |Sum ll -> (match ll with
      | [] -> Const 0
      | [a] -> diff (a,x)
      | h::t -> Sum [diff (h,x); diff (Sum t,x)])
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
    | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
  |(b1,b2) -> if balancing b1 && balancing b2 then (if torque b1=torque b2 then true else false) else false
    and torque bran =
      match bran with
      |SimpleBranch (len,wei) -> len * wei
      |CompoundBranch (len,mob) -> len * (torque2 mob)
    and torque2 immo =
      match immo with
      | (a,b) -> torque3 a + torque3 b
    and torque3 branw =
      match branw with
      | SimpleBranch (_,a) -> a
      | CompoundBranch (_,a) -> torque2 a
    and balancing branc =
      match branc with
      |SimpleBranch (l,w) -> true
      |CompoundBranch (l,m) -> balanced m
end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun a -> match a with
  | X -> raise (Failure "X is ambiguous")
  | INT n -> n
  | ADD (n1,n2) -> calculator n1 + calculator n2
  | SUB (n1,n2) -> calculator n1 - calculator n2
  | MUL (n1,n2) -> calculator n1 * calculator n2
  | DIV (n1,n2) -> calculator n1 / calculator n2
  | SIGMA (first ,second ,exp3) ->
    let one = calculator first and two = calculator second in
      if one > two then 0 else sigma (exp3,first) + calculator( SIGMA (ADD (first,INT 1),second, exp3))
    and sigma (equ,nn) =
    match equ with
    |X -> calculator nn
    |INT n -> n
    |ADD (n1,n2) -> sigma(n1,nn) + sigma (n2,nn)
    |SUB (n1,n2) -> sigma(n1,nn) - sigma (n2,nn)
    |MUL (n1,n2) -> sigma(n1,nn) * sigma (n2,nn)
    |DIV (n1,n2) -> sigma(n1,nn) / sigma (n2,nn)
    |SIGMA (n1,n2,eqq) -> calculator (SIGMA (INT(sigma(n1,nn)),INT(sigma(n2,nn)),eqq))
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp = V of var
            |P of var * exp
            |C of exp * exp
  and var = string

  let rec lcheck: exp * var list -> bool
     = fun (exp, vlist) ->
     match exp with
       | V(var1) -> (match vlist with
                     | [] -> false
                     | hd :: tl -> if hd = var1 then true else lcheck(exp, tl))
       | P(var1, exp1) -> lcheck(exp1,  var1 :: vlist)
       | C(exp1, exp2) -> lcheck(exp1, vlist) && lcheck(exp2, vlist)

  let check : exp -> bool
     = fun exp -> lcheck(exp, [])
end
