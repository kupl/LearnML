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
	|Const i -> Const 0
	|Var s -> if s=var then Const 1 else Var s
	|Power(s,i)->if s=var then Times[Const i;Power(s,(i-1))] else Power(s,i)
	|Sum lst ->(match lst with |[]->Const 0 |hd::tl ->Sum[diff(hd,var);diff(Sum tl,var)])
	|Times lst ->(match lst with|[]->Const 0 |hd::tl ->if diff(hd,var)=Const 0 then Times[hd;diff(Times tl,var)] else Sum[Times([diff(hd,var)]@tl);Times[hd;diff(Times tl,var)]]) ;; (* TODO *)
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

  let rec we : branch -> int =fun b -> match b with |SimpleBranch(l,w)->w |CompoundBranch(l,(b1,b2))->we b1 + we b2;;
  let rec  balanced : mobile -> bool
  = fun mob -> match mob with |(SimpleBranch(l,w),SimpleBranch(l1,w1))-> if l*w=l1*w1 then true else false |(SimpleBranch(l,w),CompoundBranch(l2,m))-> if l*w=l2*we(CompoundBranch(l2,m))&&balanced(m) then true else false | (CompoundBranch(l2,m),SimpleBranch(l,w))->if l*w=l2*we(CompoundBranch(l2,m))&&balanced(m) then true else false |(CompoundBranch(l,m),CompoundBranch(l1,m1))->if l*we(CompoundBranch(l,m))=l1*we(CompoundBranch(l1,m1))&&balanced(m)&&balanced(m1) then true else false;; (* TODO *)
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
  = fun exp -> let rec value : exp->exp->int = fun e1 e2 ->( match e1 with
	| X -> value e2 e2
	| INT i -> i
	| ADD(x,y) -> value x e2 + value y e2
	| SUB(x,y) -> value x e2 - value y e2
	| MUL(x,y) -> value x e2 * value y e2
	| DIV(x,y) -> value x e2 / value y e2
	| SIGMA(x,y,z) ->let rec si : exp -> int ->int -> int = fun n x y ->
	if x<y then value n  (INT x) + si n (x+1) y 
	else value n (INT x) in si z (value x e2) (value y e2)) in value exp (INT 0);;  (* TODO *)
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

  let rec input_var : exp -> string list -> string list = fun e lst ->match e with |V(v)->lst |P(v,e1)->(input_var e1 lst)@[v] |C(e1,e2)->(input_var e1 (input_var e2 lst)) ;;
  let rec input_exp : exp->string list -> string list = fun e lst -> match e with |V(v)->lst@[v] |P(v,e1) -> input_exp e1 lst |C(e1,e2)->input_exp e1 (input_exp e2 lst) ;;
  let rec isthere : string list -> string list -> bool = fun lst l2 -> match lst with |[]->true |hd::tl->(match l2 with |[]->false |h::t->if hd=h then true else isthere [hd] t);;
  let rec check : exp -> bool
  = fun exp -> let l2=input_var exp [] in let l1 = input_exp exp [] in match exp with |V(v) -> false |C(e1,e2)->check e1 && check e2
	|P(v,e)->( match l1 with |[]->true |hd::tl -> if isthere [hd] l2 then isthere tl l2  else false);; (* TODO *)

end

