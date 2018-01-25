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

  let rec diff exp var = 
  match exp with
  | Const(i) -> Const(0)
  | Var(a) -> 
    if a=var then Const(1)
    else Const(0)
  | Power(a,i) ->
    if (a=var)=false then Const(0)
    else if i==0 then Const(0)
    else if i==1 then Var(a)
    else Times([Const i;Power(a, i-1)]);;
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

  let rec tweight brch = 
 match brch with
 | SimpleBranch(l,w) -> w
 | CompoundBranch(l,(b1, b2)) -> (tweight b1) + (tweight b2)

let rec tqcal brch = 
 match brch with
 | SimpleBranch(l,w) -> l*w
 | CompoundBranch(l,(b1, b2)) -> l*((tweight b1) + (tweight b2));;

let balanced mob = 
 match mob with
 | (b1,b2) -> 
   if tqcal(b1)==tqcal(b2) then true
   else false;;
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

  let rec replace(e1,e3) = 
 match e3 with
 | X -> e1
 | INT(a) -> INT(a)
 | ADD(ea,eb) -> ADD(replace(e1,ea),replace(e1,eb))
 | SUB(ea,eb) -> SUB(replace(e1,ea),replace(e1,eb))
 | MUL(ea,eb) -> MUL(replace(e1,ea),replace(e1,eb))
 | DIV(ea,eb) -> DIV(replace(e1,ea),replace(e1,eb))
 | SIGMA(ea,eb,ec) -> SIGMA(replace(e1,ea),replace(e1,eb),replace(e1,ec));;

let rec calculator exp = 
 match exp with
 | INT(a) -> a
 | ADD(e1,e2) -> calculator(e1)+calculator(e2)
 | SUB(e1,e2) -> calculator(e1)-calculator(e2)
 | MUL(e1,e2) -> calculator(e1)*calculator(e2)
 | DIV(e1,e2) -> calculator(e1)/calculator(e2)
 | SIGMA(e1,e2,e3) -> 
   if calculator(e1)=calculator(e2) then calculator(replace(e1,e3))
   else calculator(ADD(replace(e1,e3),SIGMA(ADD(e1,INT(1)),e2,e3)));;
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


