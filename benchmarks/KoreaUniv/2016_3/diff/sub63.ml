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

	let rec diff_sum : aexp list * string -> aexp list
	= fun (aexplist, var) -> match aexplist with
	| [] -> [Const 0]
	| hd::tl -> [diff(hd, var)]@(diff_sum(tl, var))

	and diff_times : aexp list * string -> aexp
	= fun (aexplist, var) -> match aexplist with
	| [] -> Const 0
	| hd::tl -> Sum [Times ([diff(hd, var)]@tl); Times [hd; diff_times(tl, var)]]

  and diff : aexp * string -> aexp
	= fun (exp, var) -> match exp with
	| Const num -> Const 0
	| Var st -> if st=var then Const 1 else Const 0
	| Power (st, num) -> if st=var then Times [Const num; Power (st, num-1)] else Const 0
	| Times aexplist -> diff_times(aexplist@[Const 1], var)
	| Sum aexplist -> Sum (diff_sum(aexplist, var))

(*  = fun (exp, var) -> raise NotImplemented*) (* TODO *)
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

	let abs : int -> int
	= fun x -> if x>0 then x else 0-x

	let rec balance_weight : mobile -> int
	= fun mob -> match mob with
	| (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> w1+w2
	| (SimpleBranch(l1, w1), CompoundBranch(l2, m2)) -> w1+balance_weight(m2)
	| (CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> balance_weight(m1)+w2
	| (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> balance_weight(m1)+balance_weight(m2)

  and balanced : mobile -> bool
	= fun mob -> match mob with
	| (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> if l1*w1 = l2*w2 then true else false
	| (SimpleBranch(l1, w1), CompoundBranch(l2, m2)) -> if l1*w1 = l2*balance_weight(m2) then balanced(m2) else false
	| (CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> if l1*balance_weight(m1) = l2*w2 then balanced(m1) else false
	| (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> if l1*balance_weight(m1) = l2*balance_weight(m2) then balanced(m1) && balanced(m2) else false

(*  = fun mob -> raise NotImplemented *) (* TODO *)
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

	let rec cal_sigma_main : int * int * exp -> int
	= fun (xstart, xend, xexp) -> if xstart<=xend then cal_body(xstart, xexp) + cal_sigma_main(xstart+1, xend, xexp) else 0

	and cal_body : int * exp -> int
	= fun (xvalue, exp) -> match exp with
	| X -> xvalue
	| INT a -> a
	| ADD (exp1, exp2) -> cal_body(xvalue, exp1) + cal_body(xvalue, exp2)
	| SUB (exp1, exp2) -> cal_body(xvalue, exp1) - cal_body(xvalue, exp2)
	| MUL (exp1, exp2) -> cal_body(xvalue, exp1) * cal_body(xvalue, exp2)
	| DIV (exp1, exp2) -> cal_body(xvalue, exp1) / cal_body(xvalue, exp2)
	| SIGMA (exp1, exp2, exp3) -> cal_sigma_main(cal_body(xvalue, exp1), cal_body(xvalue, exp2), exp3)

  and calculator : exp -> int
	= fun exp -> cal_body(0, exp)

(*  = fun exp -> raise NotImplemented *) (* TODO *)
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

	let rec deletefrom : var list * var -> var list
	= fun (varlist1, vardel) -> match varlist1 with
	| [] -> []
	| hd::tl -> if hd=vardel then deletefrom(tl, vardel) else [hd]@(deletefrom(tl, vardel))

	and addfor : var list * var -> var list
	= fun (varlist1, varadd) -> match varlist1 with
	| [] -> [varadd]
	| hd::tl -> if hd=varadd then varlist1 else [hd]@(addfor(tl, varadd))

	and check2 : exp * var list -> var list
	= fun (exp, varlist1) -> match exp with
	| V var1 -> addfor(varlist1, var1)
	| P (var1, exp1) -> deletefrom(check2(exp1, varlist1), var1)
	| C (exp1, exp2) -> (check2(exp1, varlist1))@(check2(exp2, varlist1))

  let check : exp -> bool
	= fun exp -> match check2(exp, []) with
	| [] -> true
	| _ -> false

(*  = fun exp -> raise NotImplemented*) (* TODO *)
end

