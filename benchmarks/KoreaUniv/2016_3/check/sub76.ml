(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented
exception FreeVariableError
exception SigLargeBottomError

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
	Const(i) -> Const(0)
	|Var(str) -> if(var = str) then Const(1) else Const(0)
	|Power(str,i) -> if(var = str) then Times([Const(i);Power(str,i-1)]) else Const(0)
	|Sum(l2) -> begin
		match l2 with
		[] -> Const(0)
		|hd::tl -> Sum([diff (hd,var);diff (Sum(tl),var)])
		end
	|Times (lst) -> begin
		match lst with
		[] -> Const(0)
		|hd::tl -> Sum([Times((diff (hd,var))::tl);Times([hd;diff (Times(tl),var)])])
		end (* TODO *)
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

  let rec calc_weight : mobile -> int
  = fun mob -> match mob with
	(SimpleBranch(l11,w11),SimpleBranch(l12,w12)) -> w11 + w12
	|(SimpleBranch(l21,w21),CompoundBranch(l22,m22)) -> w21 + (calc_weight m22)
	|(CompoundBranch(l31,m31),SimpleBranch(l32,w32)) -> (calc_weight m31) + w32
	|(CompoundBranch(l41,m41),CompoundBranch(l42,m42)) -> (calc_weight m41) + (calc_weight m42)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
	(SimpleBranch(l11,w11),SimpleBranch(l12,w12)) -> if((l11*w11) = (l12*w12)) then true else false
	|(SimpleBranch(l21,w21),CompoundBranch(l22,m22)) -> if((balanced m22) && ((l21*w21) = (l22*(calc_weight m22)))) then true else false
	|(CompoundBranch(l31,m31),SimpleBranch(l32,w32)) -> if((balanced m31) && ((l32*w32) = (l31*(calc_weight m31)))) then true else false
	|(CompoundBranch(l41,m41),CompoundBranch(l42,m42)) -> if((balanced m41) && (balanced m42) && ((l41*(calc_weight m41)) = (l42*(calc_weight m42)))) then true else false (* TODO *)
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

  let rec calc_parse : exp * int -> exp
  = fun (e3, i) -> match e3 with
	X -> INT(i)
	|INT(a) -> INT(a)
	|ADD(ea,eb) -> ADD(calc_parse (ea,i), calc_parse (eb,i))
	|SUB(ea,eb) -> SUB(calc_parse (ea,i),calc_parse (eb,i))
	|MUL(ea,eb) -> MUL(calc_parse (ea,i),calc_parse (eb,i))
	|DIV(ea,eb) -> DIV(calc_parse (ea,i),calc_parse (eb,i))
	|SIGMA(ea,eb,ec) -> SIGMA(calc_parse (ea,i),calc_parse (eb,i),ec)

  let rec calculator : exp -> int
  = fun exp -> match exp with
	X -> raise FreeVariableError
	|INT(i) -> i
	|ADD(e1,e2) -> (calculator e1) + (calculator e2)
	|SUB(e1,e2) -> (calculator e1) - (calculator e2)	
	|MUL(e1,e2) -> (calculator e1) * (calculator e2)
	|DIV(e1,e2) -> (calculator e1) / (calculator e2)
	|SIGMA(e1,e2,e3) -> if((calculator e1) > (calculator e2)) then (raise SigLargeBottomError)
				else (if((calculator e1) = (calculator e2)) then (calculator (calc_parse (e3, calculator e1)))
					else ((calculator (calc_parse (e3, calculator e1))) + (calculator (SIGMA(ADD(e1,INT(1)),e2,e3))))) (* TODO *)
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

  let rec has_env : var list -> var -> bool
  = fun li v -> match li with
	[] -> false
	|hd::tl -> if(hd = v) then true else (has_env tl v)

  let rec checklist : exp -> var list -> bool
  = fun exp l -> match exp with
	V(v) -> has_env l v
	|P(v,e) -> let li = v::l in (checklist e li)
	|C(e1,e2) -> (checklist e1 l) && (checklist e2 l)

  let check : exp -> bool
  = fun exp -> let li = [] in checklist exp li (* TODO *)
end

