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

  let rec  diff : aexp * string -> aexp
  = fun (exp, var) ->
match exp with
| Const n -> Const 0
| Var k -> if k = var then Const 1 else Const 1 
| Power (k,n) -> if k = var then  Times [Const n; Power (k,n-1)] else Const 0 
| Times (hd::tl) ->if (tl=[]) then diff(hd,var) else if diff(hd,var) = Const 0 then Times[hd;diff(Times (tl),var)] else Times [diff(hd,var);diff(Times (tl),var)]
| Sum (hd::tl) -> if(tl = []) then diff(hd,var) else Sum [diff(hd,var); diff(Sum (tl),var)]
| _ -> raise NotImplemented (* TODO *)
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

  let rec balanced : mobile -> bool
  = fun mob ->
begin
match mob with
| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) ->  if (l1*w1) = (l2*w2) then true else false
| (CompoundBranch(l1,mb),SimpleBranch(l2,w)) -> if balanced(mb) then if eval(mb)*l1 = l2*w then true else false else false
| (SimpleBranch(l1,w),CompoundBranch(l2,mb)) -> if balanced(mb) then if eval(mb)*l2 = l1*w then true else false else false
| (CompoundBranch(l1,mb1),CompoundBranch(l2,mb2)) ->if balanced (mb1) && balanced (mb2) then if eval(mb1)*l1 = eval(mb2)*l2 then true else false else false
end

and eval : mobile -> int
 = fun mob ->
match mob with
| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
| (CompoundBranch(l1,mb),SimpleBranch(l2,w2)) -> eval(mb)+w2
| (SimpleBranch(l1,w1),CompoundBranch(l2,mb)) -> w1 + eval(mb)
| (CompoundBranch(l1,mb1),CompoundBranch(l2,mb2)) -> eval(mb1) + eval(mb2)


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
  = fun exp ->
match exp with
| X -> 0
| INT a -> a
| ADD (a,b) -> calculator(a) + calculator (b)
| SUB (a,b) -> calculator(a) - calculator (b)
| MUL (a,b) -> calculator(a) * calculator (b)
| DIV (a,b) -> calculator(a) / calculator (b)
| SIGMA (i,k,ep) ->if calculator(i) = calculator(k) then calculator(eval2(ep,calculator(k))) else  calculator(eval2(ep,calculator(i))) + calculator( SIGMA(ADD(INT 1, i),k ,ep))



and eval2 : exp * int -> exp
=fun (exp,i) ->
match exp with
| X -> INT i
| INT a -> INT a
| ADD (a,b) -> ADD(eval2(a,i),eval2(b,i))
| SUB (a,b) -> SUB(eval2(a,i),eval2(b,i))
| MUL (a,b) -> MUL(eval2(a,i),eval2(b,i))
| DIV (a,b) -> DIV(eval2(a,i),eval2(b,i))


| _ -> raise NotImplemented  (* TODO *)


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

  let rec check : exp -> bool
  = fun exp ->
match exp with
| V v -> true
| P (v,p) -> true 
| _ -> raise NotImplemented (* TODO *)



end

